#!/usr/bin/env python3
"""Job-control tests for posh, driven through a real pty.

Job control is the one subsystem that cannot be tested in-image or with a
plain pipe: it needs a controlling terminal so that tcsetpgrp() succeeds, the
shell takes the job-control path, and Ctrl-Z reaches the foreground process
group. This harness allocates a pty, puts posh in its own session with that
pty as the controlling terminal, and drives it like a user would.

    ./test/jobs-pty.py [path-to-posh]        (default: ./posh)
"""

import os
import pty
import re
import select
import signal
import sys
import termios
import time

POSH = sys.argv[1] if len(sys.argv) > 1 else "./posh"
PROMPT = "$ "

passed = 0
failed = 0


def fail(name, detail):
    global failed
    failed += 1
    print(f"FAIL {name}\n  {detail}")


def ok(name):
    global passed
    passed += 1


class Shell:
    """An interactive posh on the far end of a pty."""

    def __init__(self, path):
        self.pid, self.fd = pty.fork()
        if self.pid == 0:  # child: pty is already our controlling terminal
            os.environ["PS1"] = PROMPT
            try:
                os.execv(path, [path])
            except Exception:
                os._exit(127)
        # Disable echo so captured output is just the shell's, not our input.
        attrs = termios.tcgetattr(self.fd)
        attrs[3] &= ~termios.ECHO
        termios.tcsetattr(self.fd, termios.TCSANOW, attrs)
        self.buf = ""
        self.dead = False
        self.read_until_quiet()

    def send(self, text):
        # EIO here means the shell is gone; report it as data rather than
        # letting an OSError abort the whole run mid-suite.
        try:
            os.write(self.fd, text.encode())
        except OSError:
            self.dead = True

    def line(self, cmd):
        self.send(cmd + "\n")

    def read_until_quiet(self, idle=0.45, limit=6.0):
        """Read until the shell produces nothing for `idle` seconds."""
        deadline = time.time() + limit
        out = ""
        while time.time() < deadline:
            r, _, _ = select.select([self.fd], [], [], idle)
            if not r:
                break
            try:
                chunk = os.read(self.fd, 65536)
            except OSError:
                break
            if not chunk:
                break
            out += chunk.decode(errors="replace")
        self.buf += out
        return out

    def drain(self):
        self.read_until_quiet()
        self.buf = ""

    def run(self, cmd):
        """Send a command and return everything printed before the next idle."""
        self.drain()
        self.line(cmd)
        return strip_prompts(self.read_until_quiet())

    def close(self):
        try:
            self.line("exit")
            self.read_until_quiet(idle=0.2, limit=1.0)
        except OSError:
            pass
        try:
            os.kill(self.pid, signal.SIGKILL)
        except ProcessLookupError:
            pass
        try:
            os.waitpid(self.pid, 0)
        except ChildProcessError:
            pass
        os.close(self.fd)


def strip_prompts(s):
    s = s.replace("\r\n", "\n")
    return "\n".join(ln for ln in s.split("\n") if ln.strip() not in ("", "$"))


def has_tty_job_control(sh):
    """Sanity gate: confirm the shell really took the job-control path."""
    out = sh.run("sleep 0.05 &")
    return bool(re.search(r"\[\d+\]\s+\d+", out))


# ---------------------------------------------------------------------------

def main():
    global passed, failed

    if not os.access(POSH, os.X_OK):
        print(f"jobs-pty: no executable at {POSH} (run: make build)", file=sys.stderr)
        return 1

    sh = Shell(POSH)
    try:
        if not has_tty_job_control(sh):
            print("jobs-pty: shell did not report a background job; "
                  "job control is not active under the pty", file=sys.stderr)
            return 1
        sh.read_until_quiet()

        # --- background job appears in the table ---------------------------
        out = sh.run("sleep 3 &")
        if re.search(r"\[\d+\]\s+\d+", out):
            ok("bg-announce")
        else:
            fail("bg-announce", f"expected '[n] pid', got {out!r}")

        out = sh.run("jobs")
        if "Running" in out and "sleep 3" in out:
            ok("jobs-running")
        else:
            fail("jobs-running", f"expected a Running sleep 3, got {out!r}")

        # --- $! tracks the background pid ----------------------------------
        out = sh.run("sleep 3 & echo bang=$!")
        if re.search(r"bang=\d+", out):
            ok("bang-pid")
        else:
            fail("bang-pid", f"expected bang=<pid>, got {out!r}")

        sh.run("kill %1 2>/dev/null; kill %2 2>/dev/null")
        sh.drain()

        # --- a backgrounded pipeline must finish and leave the table -------
        sh.run("sleep 0.2 | cat &")
        time.sleep(1.2)
        sh.run("")               # a prompt cycle: poll_jobs + notify runs here
        out = sh.run("jobs")
        if "Running" in out:
            fail("pipeline-completes",
                 f"backgrounded pipeline still Running after it exited: {out!r}")
        else:
            ok("pipeline-completes")

        # --- Ctrl-Z stops a foreground job ---------------------------------
        sh.drain()
        sh.line("sleep 5")
        time.sleep(0.5)
        sh.send("\x1a")          # Ctrl-Z -> SIGTSTP to the foreground pgroup
        out = strip_prompts(sh.read_until_quiet())
        if "Stopped" in out:
            ok("ctrl-z-stops")
        else:
            fail("ctrl-z-stops", f"expected 'Stopped', got {out!r}")

        out = sh.run("jobs")
        if "Stopped" in out:
            ok("jobs-stopped")
        else:
            fail("jobs-stopped", f"expected a Stopped job, got {out!r}")

        # --- bg resumes it (SIGCONT) ---------------------------------------
        # Checking that `jobs` prints "Running" here would be vacuous: the bg
        # builtin sets that state unconditionally, so the table says Running
        # even when SIGCONT never arrived. Instead give the job a deadline and
        # confirm it actually ran to completion -- a still-stopped process
        # never finishes, so this fails if the wrong signal is sent.
        sh.drain()
        sh.line("sleep 1")
        time.sleep(0.4)
        sh.send("\x1a")
        out = strip_prompts(sh.read_until_quiet())
        if "Stopped" not in out:
            fail("bg-setup", f"could not stop the job to resume: {out!r}")
        else:
            sh.run("bg")
            time.sleep(2.5)          # >> the 1s the job needs once resumed
            sh.run("")               # prompt cycle: poll_jobs + notify
            out = sh.run("jobs")
            # Look only at this job's line: earlier tests may have left other
            # jobs in the table, and their state must not colour this verdict.
            mine = [l for l in out.split("\n") if l.rstrip().endswith("sleep 1")]
            if not mine:
                ok("bg-resumes")     # ran to completion and left the table
            elif "Stopped" in mine[0]:
                fail("bg-resumes",
                     f"bg left the job Stopped -- wrong signal for SIGCONT? {mine[0]!r}")
            else:
                fail("bg-resumes",
                     f"job never finished after bg: {mine[0]!r}")

        # --- fg brings it back and waits -----------------------------------
        sh.drain()
        sh.line("sleep 5")
        time.sleep(0.4)
        sh.send("\x1a")
        sh.read_until_quiet()
        sh.run("bg")
        time.sleep(0.3)
        out = sh.run("fg")
        if "sleep 5" in out:
            ok("fg-reports-command")
        else:
            fail("fg-reports-command", f"expected fg to echo the command, got {out!r}")
        # fg is now blocking on sleep 5; interrupt it so the shell stays usable
        sh.send("\x03")
        sh.read_until_quiet()

        # --- a bare job number is a job spec, not a pid ---------------------
        sh.drain()
        sh.run("sleep 4 &")
        out = sh.run("jobs")
        m = re.search(r"\[(\d+)\][+\- ]\s+Running\s+sleep 4", out)
        if not m:
            fail("fg-bare-number", f"could not find the job to reference: {out!r}")
        else:
            n = m.group(1)
            out = sh.run(f"fg {n}")          # `fg 1`, not `fg %1`
            if "no such job" in out:
                fail("fg-bare-number",
                     f"`fg {n}` did not resolve to job {n} (read as a pid?): {out!r}")
            else:
                ok("fg-bare-number")
            sh.send("\x03")                  # stop waiting on it
            sh.read_until_quiet()

        # --- an externally resumed job is noticed (WCONTINUED) -------------
        # 'sleep 300' so the job cannot finish on its own during the several
        # seconds of pty round-trips below -- a shorter sleep expires and shows
        # up as Done, which looks exactly like a missed continue event.
        sh.drain()
        sh.run("sleep 300 &")
        sh.run("kill -STOP $!")
        out = sh.run("jobs")
        mine = [l for l in out.split("\n") if l.rstrip().endswith("sleep 300")]
        if not mine or "Stopped" not in mine[0]:
            fail("wcontinued", f"job did not register as Stopped: {out!r}")
        else:
            sh.run("kill -CONT $!")
            time.sleep(0.4)
            out = sh.run("jobs")
            mine = [l for l in out.split("\n") if l.rstrip().endswith("sleep 300")]
            if mine and "Running" in mine[0]:
                ok("wcontinued")
            else:
                fail("wcontinued",
                     f"resumed job not back to Running -- WCONTINUED missing? "
                     f"{(mine or out)!r}")
        sh.run("kill -KILL $! 2>/dev/null")
        sh.drain()

        # --- job control works with stderr redirected (/dev/tty) -----------
        # Terminal ownership must not go through fd 2: `fg` with stderr sent
        # elsewhere would otherwise tcsetpgrp() the wrong file and leave the
        # terminal owned by a dead process group.
        sh.drain()
        sh.line("sleep 0.4")
        time.sleep(0.3)
        sh.send("\x1a")
        sh.read_until_quiet()
        sh.run("fg %1 2>/dev/null")
        time.sleep(0.8)
        out = sh.run("echo tty-ok")
        if "tty-ok" in out:
            ok("tty-redirected-stderr")
        else:
            fail("tty-redirected-stderr",
                 f"shell lost the terminal after fg with stderr redirected: {out!r}")

        # --- Ctrl-C at an idle prompt must not kill the shell ---------------
        # POSIX: an interactive shell catches SIGINT and returns to the prompt.
        # With the default disposition the shell is simply terminated.
        sh.drain()
        sh.send("\x03")
        time.sleep(0.4)
        out = sh.run("echo after-int")
        if "after-int" in out:
            ok("sigint-at-prompt")
        else:
            fail("sigint-at-prompt",
                 f"shell did not survive Ctrl-C at the prompt: {out!r}")

        # --- Ctrl-\ likewise ------------------------------------------------
        sh.drain()
        sh.send("\x1c")                      # Ctrl-backslash -> SIGQUIT
        time.sleep(0.4)
        out = sh.run("echo after-quit")
        if "after-quit" in out:
            ok("sigquit-at-prompt")
        else:
            fail("sigquit-at-prompt",
                 f"shell did not survive Ctrl-\\ at the prompt: {out!r}")

        # --- the shell survived all of that --------------------------------
        out = sh.run("echo still-alive")
        if "still-alive" in out:
            ok("shell-survives")
        else:
            fail("shell-survives", f"shell unresponsive after job control: {out!r}")
    finally:
        sh.close()

    print(f"\njobs-pty: {passed} passed, {failed} failed")
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())

#!/usr/bin/env python3
"""History and `fc`, driven through a real pty.

    test/history-pty.py [path-to-sxsh]

History is only recorded at an interactive prompt, and a shell is only
interactive when it has a terminal -- so none of this can be reached from
`sxsh -c` or a pipe. That is the same reason test/jobs-pty.py exists, and this
file follows its shape: fork a pty, drive the shell, assert on what came back.

Every bug this file pins was found by running the shell, not by reading it:

  * history was never recorded at all -- `history-add` had no caller, so
    `history` printed nothing and `fc` always said "history is empty";
  * `exit` did not exit an interactive shell, because RUN swallows SHELL-EXIT
    as a top-level backstop, so the REPL's handler never saw it -- which also
    meant $HISTFILE was never written;
  * `fc -s` re-executed ITSELF forever, because the REPL records a line before
    running it and `fc -s` with no operand resolves to the most recent entry.
"""

import os
import pty
import re
import select
import sys
import tempfile
import time

passed = 0
failed = 0


def ok(name):
    global passed
    passed += 1
    print("  ok   %s" % name)


def fail(name, detail):
    global failed
    failed += 1
    print("  FAIL %s\n       %s" % (name, detail.replace("\n", "\n       ")))


class Shell:
    """An sxsh running under a pty, with $HISTFILE pointed at a scratch file."""

    def __init__(self, path, histfile, env=None):
        self.pid, self.fd = pty.fork()
        if self.pid == 0:
            os.environ["PS1"] = "$ "
            os.environ["HISTFILE"] = histfile
            os.environ["TERM"] = "dumb"
            if env:
                os.environ.update(env)
            os.execv(path, [path])
            os._exit(1)
        time.sleep(0.4)

    def send(self, text):
        os.write(self.fd, text.encode("utf8"))

    def read_until_quiet(self, idle=0.4, limit=8.0):
        out = b""
        deadline = time.time() + limit
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
            out += chunk
        return out.decode("utf8", "replace")

    def wait_for_exit(self, limit=5.0):
        """True if the shell terminated on its own within LIMIT."""
        deadline = time.time() + limit
        while time.time() < deadline:
            r, _, _ = select.select([self.fd], [], [], 0.3)
            if not r:
                continue
            try:
                if not os.read(self.fd, 65536):
                    return True
            except OSError:
                return True
        return False

    def close(self):
        try:
            os.kill(self.pid, 9)
        except OSError:
            pass
        try:
            os.waitpid(self.pid, 0)
        except OSError:
            pass


def clean(text):
    """Normalise a pty transcript so assertions can read like shell output.

    Drops CRs and prompt-only lines, and strips a leading prompt from lines
    where the shell printed the prompt and the command's output back to back --
    `$     4  echo c'. Without that strip, anything anchored to the start of a
    line silently misses every first line of output."""
    text = text.replace("\r\n", "\n").replace("\r", "\n")
    lines = []
    for l in text.split("\n"):
        while l.startswith("$ ") or l == "$":
            l = l[2:] if l.startswith("$ ") else ""
        if l.strip():
            lines.append(l)
    return "\n".join(lines)


def history_lines(text):
    """Just the numbered entries of a `history' listing.

    Everything else in a pty transcript is the terminal echoing what we typed,
    so asserting on the raw text finds our own input and reports a pass or a
    failure that has nothing to do with the shell."""
    return [m.group(1)
            for m in re.finditer(r"^\s*\d+\s\s(.*)$", text, re.M)]


def session(path, histfile, script, env=None):
    sh = Shell(path, histfile, env)
    sh.send(script)
    out = sh.read_until_quiet()
    sh.close()
    return clean(out)


def main():
    path = os.path.abspath(sys.argv[1] if len(sys.argv) > 1 else "./bin/sxsh")
    if not os.access(path, os.X_OK):
        sys.exit("history-pty: no executable at %s (run: make build)" % path)

    tmp = tempfile.mkdtemp(prefix="sxsh-hist-")
    hist = os.path.join(tmp, "histfile")

    # --- recording -------------------------------------------------------
    out = session(path, hist, "echo one\necho two\nhistory\nexit\n")
    if "1  echo one" in out and "2  echo two" in out:
        ok("history records interactive commands")
    else:
        fail("history records interactive commands", out)

    # `history' numbers with %5d and two spaces; `fc -l' uses NUMBER<TAB><SPACE>.
    # They are different formats in bash and must not share a formatter.
    out = session(path, hist, "echo one\nfc -l\nexit\n")
    if re.search(r"^\d+\t echo one$", out, re.M):
        ok("fc -l uses NUMBER<TAB><SPACE>")
    else:
        fail("fc -l uses NUMBER<TAB><SPACE>", out)

    out = session(path, hist, "echo one\nfc -l -n\nexit\n")
    if re.search(r"^\t echo one$", out, re.M):
        ok("fc -l -n keeps the leading whitespace")
    else:
        fail("fc -l -n keeps the leading whitespace", out)

    # --- exit actually exits ---------------------------------------------
    sh = Shell(path, hist)
    sh.send("echo hi\nexit\n")
    if sh.wait_for_exit():
        ok("exit terminates an interactive shell")
    else:
        fail("exit terminates an interactive shell", "still running after 5s")
    sh.close()

    # --- persistence ------------------------------------------------------
    hist2 = os.path.join(tmp, "histfile2")
    session(path, hist2, "echo alpha\necho beta\nexit\n")
    if os.path.exists(hist2) and "echo alpha" in open(hist2).read():
        ok("$HISTFILE written on exit")
    else:
        fail("$HISTFILE written on exit",
             "file missing or empty: %s" % hist2)

    out = session(path, hist2, "history\nexit\n")
    if "echo alpha" in out and "echo beta" in out:
        ok("$HISTFILE loaded at startup")
    else:
        fail("$HISTFILE loaded at startup", out)

    # Numbering continues across the load rather than restarting at 1.
    if re.search(r"^\s*3\s", out, re.M):
        ok("history numbering continues across sessions")
    else:
        fail("history numbering continues across sessions", out)

    # --- fc -s ------------------------------------------------------------
    # The killer case: `fc -s' with no operand must re-run the PREVIOUS
    # command, not itself. POSIX removes the fc invocation from the list.
    hist3 = os.path.join(tmp, "histfile3")
    sh = Shell(path, hist3)
    sh.send("echo alpha\nfc -s\n")
    out = clean(sh.read_until_quiet(limit=6.0))
    sh.close()
    if out.count("alpha") >= 2 and out.count("fc -s") < 3:
        ok("fc -s re-runs the previous command, not itself")
    else:
        fail("fc -s re-runs the previous command, not itself",
             "looks like a loop:\n" + out[:400])

    hist4 = os.path.join(tmp, "histfile4")
    out = session(path, hist4, "echo alpha\nfc -s alpha=BETA\nexit\n")
    if "BETA" in out:
        ok("fc -s applies old=new")
    else:
        fail("fc -s applies old=new", out)

    # The fc invocation is replaced in history by what it actually ran.
    out = session(path, os.path.join(tmp, "h5"),
                  "echo alpha\nfc -s alpha=BETA\nhistory\nexit\n")
    entries = history_lines(out)
    if "echo BETA" in entries and not any(e.startswith("fc ") for e in entries):
        ok("fc replaces itself in history")
    else:
        fail("fc replaces itself in history", repr(entries))

    # --- HISTSIZE ---------------------------------------------------------
    out = session(path, os.path.join(tmp, "h6"),
                  "HISTSIZE=2\necho a\necho b\necho c\nhistory\nexit\n")
    entries = history_lines(out)
    if len(entries) == 2 and "echo c" in entries and "echo a" not in entries:
        ok("HISTSIZE trims the oldest entries")
    else:
        fail("HISTSIZE trims the oldest entries", repr(entries))

    print("\nhistory-pty: %d passed, %d failed" % (passed, failed))
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())

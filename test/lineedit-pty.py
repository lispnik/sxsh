#!/usr/bin/env python3
"""The line editor, driven through a real pty.

    test/lineedit-pty.py [path-to-sxsh]

Every case asserts on THE COMMAND THAT ACTUALLY RAN -- almost always by making
the shell `echo' something -- never on the redraw bytes. The redraw's exact
escape sequences are an internal choice and change whenever the painter is
tuned; what must not change is which command the user ended up executing.

Keys are sent as raw bytes with a small gap between chunks, because the editor
processes one key at a time and an ESC arriving in the same read as its
successor is legitimately a Meta prefix rather than a bare escape.
"""

import os
import pty
import re
import select
import struct
import sys
import termios
import time
import fcntl
import tempfile

passed = 0
failed = 0

C_A = b"\x01"
C_B = b"\x02"
C_D = b"\x04"
C_E = b"\x05"
C_F = b"\x06"
C_G = b"\x07"
C_K = b"\x0b"
C_L = b"\x0c"
C_N = b"\x0e"
C_P = b"\x10"
C_R = b"\x12"
C_T = b"\x14"
C_U = b"\x15"
C_W = b"\x17"
C_Y = b"\x19"
C_UNDO = b"\x1f"
DEL = b"\x7f"
UP = b"\x1b[A"
DOWN = b"\x1b[B"
LEFT = b"\x1b[D"
RIGHT = b"\x1b[C"
HOME = b"\x1b[H"
END = b"\x1b[F"
M_B = b"\x1bb"
M_F = b"\x1bf"
M_D = b"\x1bd"


def ok(name):
    global passed
    passed += 1
    print("  ok   %s" % name)


def fail(name, detail):
    global failed
    failed += 1
    print("  FAIL %s\n       %s" % (name, str(detail)[:400]))


ANSI = re.compile(r"\x1b\[[0-9;?]*[ -/]*[@-~]")


class Shell:
    def __init__(self, path, histfile, cols=80, rows=24, term="xterm"):
        self.pid, self.fd = pty.fork()
        if self.pid == 0:
            os.environ["PS1"] = "$ "
            os.environ["PS2"] = "> "
            os.environ["TERM"] = term
            os.environ["HISTFILE"] = histfile
            os.execv(path, [path])
            os._exit(1)
        self.resize(rows, cols)
        time.sleep(0.6)

    def resize(self, rows, cols):
        fcntl.ioctl(self.fd, termios.TIOCSWINSZ,
                    struct.pack("HHHH", rows, cols, 0, 0))

    def send(self, *chunks, gap=0.3):
        for c in chunks:
            os.write(self.fd, c)
            time.sleep(gap)

    def read(self, idle=0.4, limit=6.0):
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
        return clean(out.decode("utf8", "replace"))

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
    """Strip escapes and CRs; drop prompt-only and prompt-prefixed noise.

    The editor repaints the whole line on every keystroke, so the transcript
    contains every intermediate state of what was typed. Asserting on that is
    asserting on the redraw. Callers look for the command's OUTPUT instead --
    a line that is not a prompt echo."""
    text = ANSI.sub("", text).replace("\r", "\n")
    return [l for l in text.split("\n") if l.strip()]


def output_lines(lines):
    """Lines that are not the editor echoing a prompt+buffer."""
    return [l for l in lines if not l.startswith("$") and not l.startswith(">")]


def session(path, hist, chunks, cols=80, gap=0.3, term="xterm"):
    sh = Shell(path, hist, cols=cols, term=term)
    sh.send(*chunks, gap=gap)
    out = sh.read()
    sh.close()
    return out


def expect_ran(name, path, hist, chunks, wanted, cols=80, gap=0.3):
    """Assert WANTED appears in the command's output (not in a prompt echo)."""
    lines = session(path, hist, list(chunks) + [b"\n", b"exit\n"], cols=cols, gap=gap)
    outs = output_lines(lines)
    if any(wanted == l.strip() for l in outs):
        ok(name)
    else:
        fail(name, "wanted %r among %r" % (wanted, outs))


def main():
    path = os.path.abspath(sys.argv[1] if len(sys.argv) > 1 else "./bin/sxsh")
    if not os.access(path, os.X_OK):
        sys.exit("lineedit-pty: no executable at %s (run: make build)" % path)
    tmp = tempfile.mkdtemp(prefix="sxsh-le-")
    h = lambda n: os.path.join(tmp, "h%s" % n)

    # Gate: prove the editor is active at all before asserting on its details.
    lines = session(path, h("gate"), [b"cho hi", C_A, b"e", b"\n", b"exit\n"])
    if any(l.strip() == "hi" for l in output_lines(lines)):
        ok("line editing is active")
    else:
        fail("line editing is active", lines)
        print("\nlineedit-pty: cannot proceed without an active editor")
        return 1

    # --- basics ----------------------------------------------------------
    expect_ran("self-insert", path, h(1), [b"echo hi"], "hi")
    expect_ran("backspace deletes", path, h(2), [b"echo hix", DEL], "hi")
    expect_ran("C-a beginning-of-line", path, h(3), [b"cho hi", C_A, b"e"], "hi")
    expect_ran("C-e end-of-line", path, h(4),
               [b"echo", C_A, C_E, b" hi"], "hi")
    expect_ran("C-b / C-f", path, h(5), [b"echo h", C_B, C_F, b"i"], "hi")
    expect_ran("left / right arrows", path, h(6),
               [b"echo h", LEFT, RIGHT, b"i"], "hi")
    expect_ran("Home / End", path, h(7),
               [b"cho hi", HOME, b"e", END], "hi")

    # --- killing and yanking ---------------------------------------------
    expect_ran("C-k kills to end", path, h(8),
               [b"echo hi there", C_A, C_F * 8, C_K], "hi")
    expect_ran("C-u kills to start", path, h(9),
               [b"zzz", C_U, b"echo hi"], "hi")
    expect_ran("C-w rubs out a word", path, h(10),
               [b"echo hi junk", C_W], "hi")
    expect_ran("C-y yanks it back", path, h(11),
               [b"echo hi junk", C_W, C_Y, C_Y], "hi junkjunk")
    expect_ran("M-d kills forward word", path, h(12),
               [b"echo junk hi", C_A, M_F, C_F, M_D], "hi")

    # --- motion by word ---------------------------------------------------
    expect_ran("M-b moves back a word", path, h(13),
               [b"echo one two", M_B, b"X"], "one Xtwo")

    # --- transpose, case --------------------------------------------------
    expect_ran("C-t transposes", path, h(14), [b"echo ba", C_B, C_T], "ab")

    # --- undo -------------------------------------------------------------
    expect_ran("C-_ undoes a kill", path, h(15),
               [b"echo hi", C_W, C_UNDO], "hi")

    # --- history ----------------------------------------------------------
    lines = session(path, h(16),
                    [b"echo first\n", UP, b"\n", b"exit\n"])
    if len([l for l in output_lines(lines) if l.strip() == "first"]) >= 2:
        ok("Up recalls the previous command")
    else:
        fail("Up recalls the previous command", output_lines(lines))

    lines = session(path, h(17),
                    [b"echo aa\n", b"echo bb\n", UP, UP, b"\n", b"exit\n"])
    if len([l for l in output_lines(lines) if l.strip() == "aa"]) >= 2:
        ok("Up twice reaches the older command")
    else:
        fail("Up twice reaches the older command", output_lines(lines))

    lines = session(path, h(18),
                    [b"echo done\n", b"echo part", UP, DOWN, b"ial", b"\n",
                     b"exit\n"])
    outs = output_lines(lines)
    if any("partial" in l for l in outs):
        ok("Down restores the partly typed line")
    else:
        fail("Down restores the partly typed line", outs)

    # --- reverse search ---------------------------------------------------
    lines = session(path, h(19),
                    [b"echo needle\n", b"echo other\n", C_R, b"needl", b"\n",
                     b"exit\n"])
    if len([l for l in output_lines(lines) if l.strip() == "needle"]) >= 2:
        ok("C-r finds an earlier command")
    else:
        fail("C-r finds an earlier command", output_lines(lines))

    lines = session(path, h(20),
                    [b"echo needle\n", b"echo keep", C_R, b"needl", C_G, b"\n",
                     b"exit\n"])
    if any(l.strip() == "keep" for l in output_lines(lines)):
        ok("C-g abandons the search and restores the line")
    else:
        fail("C-g abandons the search and restores the line",
             output_lines(lines))

    # --- continuation lines use PS2 ---------------------------------------
    lines = session(path, h(21),
                    [b"for i in 1 2\n", b"do echo $i; done\n", b"exit\n"])
    outs = output_lines(lines)
    if any(l.strip() == "1" for l in outs) and any(l.strip() == "2" for l in outs):
        ok("multi-line command via PS2 continuation")
    else:
        fail("multi-line command via PS2 continuation", outs)
    if any(l.startswith(">") for l in lines):
        ok("PS2 is actually printed")
    else:
        fail("PS2 is actually printed", lines[:12])

    # --- narrow terminal --------------------------------------------------
    expect_ran("wraps correctly at 20 columns", path, h(22),
               [b"echo " + b"x" * 40], "x" * 40, cols=20)

    # --- interrupt --------------------------------------------------------
    sh = Shell(path, h(23))
    sh.send(b"echo SHOULDNOTRUN", b"\x03", gap=0.35)
    sh.send(b"echo ok\n", b"exit\n", gap=0.35)
    lines = sh.read()
    sh.close()
    outs = output_lines(lines)
    if any(l.strip() == "ok" for l in outs) and not any(
            l.strip() == "SHOULDNOTRUN" for l in outs):
        ok("C-c discards the line without running it")
    else:
        fail("C-c discards the line without running it", outs)

    # --- C-d --------------------------------------------------------------
    expect_ran("C-d mid-line deletes forward", path, h(24),
               [b"echo hXi", C_B, C_B, C_D], "hi")

    sh = Shell(path, h(25))
    sh.send(C_D, gap=0.4)
    exited = False
    deadline = time.time() + 4
    while time.time() < deadline:
        r, _, _ = select.select([sh.fd], [], [], 0.3)
        if r:
            try:
                if not os.read(sh.fd, 65536):
                    exited = True
                    break
            except OSError:
                exited = True
                break
    sh.close()
    if exited:
        ok("C-d on an empty line exits")
    else:
        fail("C-d on an empty line exits", "still running")

    # --- the fallback reader still works ----------------------------------
    lines = session(path, h(26), [b"set +o emacs\n", b"echo hi\n", b"exit\n"])
    if any(l.strip() == "hi" for l in output_lines(lines)):
        ok("set +o emacs falls back to the plain reader")
    else:
        fail("set +o emacs falls back to the plain reader", output_lines(lines))

    # --- history is still recorded through the editor ----------------------
    lines = session(path, h(27),
                    [b"echo one\n", b"echo two\n", b"history\n", b"exit\n"])
    joined = "\n".join(lines)
    if "echo one" in joined and "echo two" in joined:
        ok("history records lines typed in the editor")
    else:
        fail("history records lines typed in the editor", lines[-8:])

    print("\nlineedit-pty: %d passed, %d failed" % (passed, failed))
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())

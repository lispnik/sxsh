#!/usr/bin/env python3
"""shell/term.lisp -- raw mode and key decoding, through a real pty.

    test/term-pty.py

None of this is reachable without a terminal: tcgetattr fails with ENOTTY on a
pipe, so these run sbcl inside a pty and drive the primitives directly. The
shell binary is not involved -- the line editor is tested separately once it
exists; this file pins the layer beneath it.

The invariant worth protecting above all others is the LAST test here: no path
may leave the terminal in raw mode. A shell that exits raw hands the user a
session with no echo and no line discipline, and it outlives the shell.
"""

import os
import pty
import select
import sys
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
    print("  FAIL %s\n       %s" % (name, str(detail)[:500]))


def run_probe(lisp, send=b"", settle=6.0, limit=20.0, send2=None, gap=0.5):
    """Run LISP inside a pty with the shell system loaded; return its output.

    SEND2 is written after GAP seconds. That gap is not padding: an ESC
    followed immediately by another byte IS a Meta prefix, and decoding it as
    one is correct. Testing a *lone* ESC therefore requires real idle time
    after it, which is exactly the condition READ-KEY's poll timeout exists to
    detect."""
    pid, fd = pty.fork()
    if pid == 0:
        os.execvp("sbcl", ["sbcl", "--noinform", "--non-interactive",
                           "--eval", "(require :asdf)",
                           "--eval", '(asdf:load-system "sxsh/shell")',
                           "--eval", lisp])
        os._exit(1)
    time.sleep(settle)
    if send:
        os.write(fd, send)
    if send2 is not None:
        time.sleep(gap)
        os.write(fd, send2)
    out = b""
    deadline = time.time() + limit
    while time.time() < deadline:
        r, _, _ = select.select([fd], [], [], 1.0)
        if not r:
            break
        try:
            chunk = os.read(fd, 65536)
        except OSError:
            break
        if not chunk:
            break
        out += chunk
    try:
        os.kill(pid, 9)
        os.waitpid(pid, 0)
    except OSError:
        pass
    return out.decode("utf8", "replace")


def field(text, key):
    """Value of a KEY=... field the probe printed."""
    for line in text.replace("\r", "\n").split("\n"):
        i = line.find(key + "=")
        if i >= 0:
            return line[i + len(key) + 1:].strip()
    return None


KEY_PROBE = r"""
(progn
  (sxsh-shell::install-editor-handlers)
  (format t "COLS=~A~%" (sxsh-shell::terminal-columns))
  (finish-output)
  (sxsh-shell::with-raw-terminal
    (loop repeat 9
          for k = (sxsh-shell::read-key)
          do (sxsh-shell::term-write (format nil "KEY=~S;" k))
             (sxsh-shell::term-flush)))
  (finish-output))
"""

RESTORE_PROBE = r"""
(flet ((flags () (let ((tio (sb-posix:tcgetattr (sxsh-shell::tty-fd))))
                   (list (and (logtest (sb-posix:termios-lflag tio)
                                       sb-posix:icanon) t)
                         (and (logtest (sb-posix:termios-lflag tio)
                                       sb-posix:echo) t)))))
  (format t "BEFORE=~S~%" (flags)) (finish-output)
  (sxsh-shell::with-raw-terminal
    (sxsh-shell::term-write (format nil "INSIDE=~S;" (flags)))
    (sxsh-shell::term-flush)
    (sxsh-shell::read-key))
  (format t "AFTER=~S~%" (flags))
  (ignore-errors (sxsh-shell::with-raw-terminal (error "boom")))
  (format t "AFTER-THROW=~S~%" (flags))
  (finish-output))
"""


def main():
    # a, C-a, Up, DEL, TAB, M-b, lone ESC, CR, C-d.
    # The lone ESC is the interesting one: without the poll timeout in READ-KEY
    # it blocks forever waiting for a sequence that never comes.
    out = run_probe(KEY_PROBE,
                    send=b"a\x01\x1b[A\x7f\t\x1bb\x1b",
                    send2=b"\r\x04")

    if field(out, "COLS"):
        ok("terminal-columns returns a width")
    else:
        fail("terminal-columns returns a width", out)

    keys = "".join(l for l in out.replace("\r", "\n").split("\n") if "KEY=" in l)
    expected = [
        ("plain character", "#\\a"),
        ("control character", "#\\Soh"),        # C-a
        ("arrow escape sequence", ":UP"),
        ("DEL is backspace", ":BACKSPACE"),
        ("tab", "#\\Tab"),
        ("meta prefix", "(:META . #\\b)"),
        ("lone ESC does not hang", ":ESCAPE"),
        ("CR not LF (ICRNL cleared)", "#\\Return"),
        ("C-d", "#\\Eot"),
    ]
    for name, want in expected:
        if want in keys:
            ok("read-key: %s" % name)
        else:
            fail("read-key: %s" % name, "wanted %s in %s" % (want, keys))

    # The safety invariant.
    out = run_probe(RESTORE_PROBE, send=b"x")
    before, inside = field(out, "BEFORE"), field(out, "INSIDE")
    after, after_throw = field(out, "AFTER"), field(out, "AFTER-THROW")

    if inside and inside.startswith("(NIL NIL)"):
        ok("raw mode clears ICANON and ECHO")
    else:
        fail("raw mode clears ICANON and ECHO", out)

    if before and after and before == after:
        ok("terminal restored after a normal exit")
    else:
        fail("terminal restored after a normal exit",
             "before=%s after=%s" % (before, after))

    if before and after_throw and before == after_throw:
        ok("terminal restored after a thrown condition")
    else:
        fail("terminal restored after a thrown condition",
             "before=%s after-throw=%s" % (before, after_throw))

    print("\nterm-pty: %d passed, %d failed" % (passed, failed))
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())

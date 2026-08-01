#!/usr/bin/env python3
"""Run the Oils spec tests with sxsh as the shell under test.

Oils' `test/sh_spec.py` drives the largest cross-shell conformance corpus that
exists (222 .test.sh files here). Each case is a shell snippet plus expected
stdout/stderr/status, run through one or more shells -- the same model as our
own test/posix-diff.sh, but far bigger and maintained by people who compare
bash, dash, mksh, zsh and yash case by case.

    test/oils-spec.py smoke word-split      # named spec files
    test/oils-spec.py --list                # what's available
    test/oils-spec.py --summary --all       # full scoreboard (slow)
    test/oils-spec.py smoke -- -v           # pass -v through to sh_spec.py

## Why this needs Python 2

sh_spec.py is Python 2 source; upstream runs it under a vendored python2, and
Oils' own build/deps.sh downloads and builds one. We do the same rather than
port it, for two reasons: patching the submodule would turn every
`git submodule update --remote` into a merge conflict, and -- more importantly
-- the obvious Python 3 workaround is wrong. Making the subprocess pipes text
mode to satisfy `p.stdin.write(str)` also enables newline translation and
utf-8 decoding on the shells' output, in a harness whose entire purpose is
byte-exact comparison. Several spec cases deliberately emit invalid UTF-8.
A shim there would quietly corrupt the very cases most worth trusting.

Get an interpreter with:  pyenv install 2.7.18
or point SXSH_PY2 at one yourself.
"""

import os
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
OILS = os.path.join(ROOT, 'third_party', 'oils')
SPEC_DIR = os.path.join(OILS, 'spec')
SPEC_BIN = os.path.join(OILS, 'spec', 'bin')
SH_SPEC = os.path.join(OILS, 'test', 'sh_spec.py')
SXSH = os.path.join(ROOT, 'sxsh')
TMP = os.path.join(os.environ.get('TMPDIR', '/tmp'), 'sxsh-oils-spec')


def find_python2():
    """Locate a Python 2 interpreter, or return None."""
    explicit = os.environ.get('SXSH_PY2')
    if explicit:
        return explicit if os.access(explicit, os.X_OK) else None

    candidates = [
        os.path.expanduser('~/.pyenv/versions/2.7.18/bin/python2.7'),
        '/usr/bin/python2.7',
        '/usr/bin/python2',
        '/usr/local/bin/python2.7',
    ]
    for c in candidates:
        if os.access(c, os.X_OK):
            return c
    for name in ('python2.7', 'python2'):
        try:
            out = subprocess.check_output(['which', name],
                                          stderr=subprocess.DEVNULL)
            return out.decode().strip()
        except (subprocess.CalledProcessError, OSError):
            pass
    return None


def spec_files():
    if not os.path.isdir(SPEC_DIR):
        return []
    return sorted(f[:-len('.test.sh')] for f in os.listdir(SPEC_DIR)
                  if f.endswith('.test.sh'))


def run_one(py2, name, extra_argv, quiet):
    path = os.path.join(SPEC_DIR, name + '.test.sh')
    if not os.path.isfile(path):
        print('oils-spec: no such spec file: %s' % name, file=sys.stderr)
        return None

    tmp_env = os.path.join(TMP, name)
    if not os.path.isdir(tmp_env):
        os.makedirs(tmp_env)

    # Two things must be on the PATH handed to the cases, or a large fraction
    # of them fail for reasons that have nothing to do with the shell:
    #
    #   spec/bin      - helper scripts the cases call directly. argv.py prints
    #                   its arguments as a Python list and is how most word
    #                   splitting cases show their result.
    #   dirname(py2)  - those helpers are `#!/usr/bin/env python2` scripts. A
    #                   pyenv python2 is not on PATH, so without this every
    #                   argv.py case produces empty output and reads as a
    #                   shell bug rather than a missing interpreter.
    path_env = os.pathsep.join(
        [os.path.dirname(py2), SPEC_BIN, os.environ.get('PATH', '/usr/bin:/bin')])

    # A per-case timeout is essential, not a nicety: sh_spec waits forever by
    # default, so a single case that hangs the shell under test blocks the
    # whole suite instead of being recorded as one failure.
    argv = [py2, SH_SPEC,
            '--tmp-env', tmp_env,
            '--path-env', path_env,
            '--timeout', os.environ.get('SXSH_SPEC_TIMEOUT', '10'),
            path, SXSH] + extra_argv

    env = dict(os.environ)
    env['PYTHONPATH'] = OILS      # sh_spec.py does `from test import spec_lib`

    if quiet:
        p = subprocess.run(argv, cwd=OILS, env=env,
                           stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        return p.stdout.decode('utf-8', 'replace')
    subprocess.call(argv, cwd=OILS, env=env)
    return ''


ANSI_RE = None


CASE_RE = None


def parse_totals(output):
    """Count per-case verdicts in sh_spec's result table.

    Counting the individual rows rather than the trailing summary, because
    sh_spec omits that summary entirely when a file has no failures -- reading
    it would score a perfect file 0/0. Rows look like

        <case#> <line#> <verdict> <description>

    colourised, so escape sequences are stripped first. Anything that is not
    'pass' (FAIL, TIMEOUT, BUG, ...) counts against us.
    """
    global ANSI_RE, CASE_RE
    import re
    if ANSI_RE is None:
        ANSI_RE = re.compile(r'\x1b\[[0-9;]*m')
        CASE_RE = re.compile(r'^\s*\d+\s+\d+\s+(\S+)\s', re.MULTILINE)
    plain = ANSI_RE.sub('', output)
    verdicts = CASE_RE.findall(plain)
    npass = sum(1 for v in verdicts if v == 'pass')
    return npass, len(verdicts) - npass, len(verdicts)


def main(argv):
    args = argv[1:]
    passthrough = []
    if '--' in args:
        i = args.index('--')
        args, passthrough = args[:i], args[i + 1:]

    if not args or args[0] in ('-h', '--help'):
        print(__doc__)
        return 0

    if not os.path.isfile(SH_SPEC):
        sys.exit("oils-spec: third_party/oils is empty. Run:\n"
                 "  git submodule update --init --depth 1 third_party/oils")

    if args[0] == '--list':
        names = spec_files()
        print('%d spec files:' % len(names))
        for n in names:
            print('  ' + n)
        return 0

    summary = False
    if args[0] == '--summary':
        summary, args = True, args[1:]

    names = spec_files() if (args and args[0] == '--all') else args
    if not names:
        return 0

    py2 = find_python2()
    if not py2:
        sys.exit("oils-spec: no Python 2 found (sh_spec.py is Python 2 source).\n"
                 "  pyenv install 2.7.18\n"
                 "  or set SXSH_PY2=/path/to/python2")

    if not os.access(SXSH, os.X_OK):
        sys.exit('oils-spec: no executable at %s (run: make build)' % SXSH)

    tot_pass = tot_fail = 0
    for name in names:
        if summary:
            out = run_one(py2, name, passthrough, quiet=True)
            if out is None:
                continue
            p, f, t = parse_totals(out)
            tot_pass += p
            tot_fail += f
            flag = '' if f == 0 else '  <-- %d failing' % f
            print('%-26s %3d/%-3d%s' % (name, p, t, flag))
        else:
            print('\n===== %s =====' % name)
            run_one(py2, name, passthrough, quiet=False)

    if summary:
        total = tot_pass + tot_fail
        pct = (100.0 * tot_pass / total) if total else 0.0
        print('\noils-spec: %d/%d cases pass (%.1f%%) across %d spec files'
              % (tot_pass, total, pct, len(names)))
        return 1 if tot_fail else 0
    return 0


if __name__ == '__main__':
    sys.exit(main(sys.argv))

#!/usr/bin/env python3
"""Run git's own test suite with sxsh as the shell, against a reference shell.

    test/git-suite.py [-r REF] [--shell PATH] [--list] [t0001-init ...]

git's t/ suite is the most demanding real shell program we have access to.
test-lib.sh and test-lib-functions.sh are ~3000 lines of deliberately portable
sh, and each tNNNN script drives them through hundreds of cases. It uses
constructs our own tests barely touch: `local' throughout, deep function
nesting, subshells inside command substitutions inside loops, traps, and here
documents feeding `read' inside `while' loops -- the exact shape that hid the
`read' buffering bug.

## Why this is differential

A raw pass count is not a conformance signal here. Some tests need helper
binaries or a filesystem feature and fail for anyone; some depend on the
environment. So every script is run twice, once under sxsh and once under a
reference shell, and only tests that PASS under the reference and FAIL under
sxsh are reported. Those are leads; everything else is noise. This is the same
discipline as test/oils-spec.py's SXSH_SHELL and test/posix-diff.sh.

## What it needs

- the third_party/git submodule (git submodule update --init third_party/git)
- an installed git, used via GIT_TEST_INSTALLED so we do not build one
- t/helper/test-tool, which does have to be compiled: test-lib.sh refuses to
  run without it. We build just that target, not all of git. It takes a few
  minutes once, then is cached in the submodule.
- templates/blt, which normally comes from a full build; we symlink the
  installed git's templates instead.
"""

import argparse
import os
import re
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
GIT = os.path.join(ROOT, 'third_party', 'git')
GIT_T = os.path.join(GIT, 't')
DEFAULT_SHELL = os.path.join(ROOT, 'bin', 'sxsh')

# Shell-heavy scripts that do not need a network, a daemon, or a build tree.
# t0000 is the test framework testing itself, which is as shell-dense as this
# corpus gets. Override by naming scripts on the command line.
DEFAULT_TESTS = [
    't0000-basic',
    't0001-init',
    't0050-filesystem',
    't0060-path-utils',
    't1006-cat-file',
    't3600-rm',
    't7501-commit-basic',
]

REF_CANDIDATES = ['/opt/homebrew/bin/bash', '/usr/local/bin/bash', '/bin/bash']

# TAP: "ok 12 - description" / "not ok 12 - description" / "ok 12 # skip ..."
TAP_RE = re.compile(r'^(not ok|ok)\s+(\d+)\s*-?\s*(.*)$')


def pick_ref():
    for c in REF_CANDIDATES:
        if os.access(c, os.X_OK):
            return c
    return None


def find_installed_git():
    for d in ('/opt/homebrew/bin', '/usr/local/bin', '/usr/bin'):
        if os.access(os.path.join(d, 'git'), os.X_OK):
            return d
    return None


def prepare(verbose=True):
    """Make the submodule runnable. Returns the GIT_TEST_INSTALLED directory."""
    if not os.path.isdir(GIT_T):
        sys.exit('git-suite: %s missing.\n'
                 '  run: git submodule update --init --depth 1 third_party/git'
                 % GIT)

    gitdir = find_installed_git()
    if not gitdir:
        sys.exit('git-suite: no installed git found to test against.')

    # 1. GIT-BUILD-OPTIONS. test-lib.sh sources it and bails without it. The
    #    target only runs sed -- it compiles nothing -- so this is cheap.
    if not os.path.exists(os.path.join(GIT, 'GIT-BUILD-OPTIONS')):
        if verbose:
            print('git-suite: generating GIT-BUILD-OPTIONS')
        subprocess.run(['make', 'GIT-BUILD-OPTIONS'], cwd=GIT,
                       stdout=subprocess.DEVNULL, check=True)

    # 2. templates/blt normally comes from a full build. The installed git
    #    already ships the same templates, so point at those.
    blt = os.path.join(GIT, 'templates', 'blt')
    if not os.path.exists(blt):
        for cand in ('/opt/homebrew/share/git-core/templates',
                     '/usr/local/share/git-core/templates',
                     '/usr/share/git-core/templates'):
            if os.path.isdir(cand):
                os.makedirs(os.path.dirname(blt), exist_ok=True)
                if os.path.islink(blt):
                    os.unlink(blt)
                os.symlink(cand, blt)
                break
        else:
            sys.exit('git-suite: no installed git templates found for templates/blt')

    # 3. test-tool. This one really does need a compiler, but only for this
    #    target -- not for all of git.
    tool = os.path.join(GIT_T, 'helper', 'test-tool')
    if not os.path.exists(tool):
        if verbose:
            print('git-suite: building t/helper/test-tool (one time, a few minutes)')
        r = subprocess.run(['make', '-j%d' % (os.cpu_count() or 4),
                            't/helper/test-tool'], cwd=GIT)
        if r.returncode != 0 or not os.path.exists(tool):
            sys.exit('git-suite: could not build t/helper/test-tool')

    return gitdir


def run_script(shell, script, gitdir, timeout):
    """Run one tNNNN script. Returns {test-name: True/False} for real results.

    Skipped tests are omitted entirely: a skip is a statement about the
    environment, and counting it either way would only add noise.
    """
    env = dict(os.environ)
    env['GIT_TEST_INSTALLED'] = gitdir
    # Keep the reference and sxsh runs from sharing a trash directory.
    env['TEST_OUTPUT_DIRECTORY'] = os.path.join(
        GIT_T, 'out-%s' % os.path.basename(shell))
    os.makedirs(env['TEST_OUTPUT_DIRECTORY'], exist_ok=True)
    try:
        p = subprocess.run([shell, './%s.sh' % script, '--no-color'],
                           cwd=GIT_T, env=env, capture_output=True,
                           text=True, errors='replace', timeout=timeout)
    except subprocess.TimeoutExpired:
        return None, 'timed out after %gs' % timeout

    results = {}
    for line in p.stdout.splitlines():
        m = TAP_RE.match(line.strip())
        if not m:
            continue
        status, _num, desc = m.groups()
        if '# skip' in desc.lower():
            continue
        # Strip a trailing TODO/known-breakage marker.
        desc = re.sub(r'\s*#.*$', '', desc).strip()
        results[desc] = (status == 'ok')
    if not results:
        head = (p.stdout + p.stderr).strip().splitlines()
        return None, (head[0] if head else 'no TAP output')
    return results, None


def main():
    ap = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument('tests', nargs='*', default=None,
                    help='test scripts, e.g. t0001-init (default: a curated set)')
    ap.add_argument('--shell', default=DEFAULT_SHELL)
    ap.add_argument('-r', '--ref', default=None, help='reference shell')
    ap.add_argument('--timeout', type=float, default=900.0)
    ap.add_argument('--list', action='store_true',
                    help='list available test scripts and exit')
    args = ap.parse_args()

    if args.list:
        for f in sorted(os.listdir(GIT_T)):
            if re.match(r'^t\d{4}-.*\.sh$', f):
                print(f[:-3])
        return 0

    if not os.access(args.shell, os.X_OK):
        sys.exit('git-suite: no executable at %s (run: make build)' % args.shell)
    ref = args.ref or pick_ref()
    if not ref:
        sys.exit('git-suite: no reference shell found')

    gitdir = prepare()
    tests = args.tests or DEFAULT_TESTS

    total_sx = total_ref = 0
    leads = []
    for name in tests:
        if not os.path.exists(os.path.join(GIT_T, name + '.sh')):
            print('  %-22s SKIP (no such script)' % name)
            continue
        ref_res, ref_err = run_script(ref, name, gitdir, args.timeout)
        sx_res, sx_err = run_script(args.shell, name, gitdir, args.timeout)
        if ref_res is None:
            print('  %-22s ref could not run: %s' % (name, ref_err))
            continue
        if sx_res is None:
            print('  %-22s sxsh could not run: %s' % (name, sx_err))
            leads.append((name, '<entire script>', sx_err))
            continue

        # Only tests the reference passes are meaningful.
        eligible = [k for k, v in ref_res.items() if v]
        passed = [k for k in eligible if sx_res.get(k)]
        total_sx += len(passed)
        total_ref += len(eligible)
        broken = [k for k in eligible if not sx_res.get(k, False)]
        for b in broken:
            leads.append((name, b, 'passes under %s' % os.path.basename(ref)))
        flag = '' if not broken else '   <-- %d lead(s)' % len(broken)
        print('  %-22s %4d/%-4d%s' % (name, len(passed), len(eligible), flag))

    if leads:
        print('\nTests passing under the reference shell but failing under sxsh:')
        for script, test, why in leads:
            print('  %-18s %s' % (script, test))

    pct = (100.0 * total_sx / total_ref) if total_ref else 0.0
    print('\ngit-suite: %d/%d (%.1f%%) of reference-passing tests, %d lead(s)'
          % (total_sx, total_ref, pct, len(leads)))
    return 1 if leads else 0


if __name__ == '__main__':
    sys.exit(main())

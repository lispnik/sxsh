#!/usr/bin/env python3
"""Fuzz the sxsh parser.

    test/fuzz-parser.py [-n ITERATIONS] [-s SEED] [--shell PATH] [--exec]

Input is fed to the shell with -n (noexec), so the parser and lexer run but
nothing is executed. That matters: a fuzzer that RAN its input would sooner or
later generate `rm -rf /'. Pass --exec only inside a sandbox you are willing
to lose.

A shell is expected to reject nonsense -- a syntax error is a pass. What we are
hunting is the shell failing in a way that is not a shell failure:

  crash    an unhandled Lisp condition, a backtrace, the debugger
  hang     no exit within the timeout
  signal   killed rather than exiting

Findings are shrunk to a minimal reproducer before being reported, and the seed
is printed so any run can be replayed exactly.
"""

import argparse
import os
import random
import re
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
DEFAULT_SHELL = os.path.join(ROOT, 'bin', 'sxsh')

# Markers that mean the *implementation* broke rather than the input being bad.
CRASH_RE = re.compile(
    r'Unhandled |Backtrace for|SB-KERNEL|SB-INT:|SB-SYS:|COMMON-LISP|'
    r'debugger invoked|INFO: Control stack|corruption|memory fault|'
    r'unhandled condition', re.I)

# Constructs worth combining. Deliberately includes the shapes that have
# historically broken shells: unbalanced quoting, here-doc delimiters at EOF,
# deep nesting, and $(( )) vs ( ) ambiguity.
FRAGMENTS = [
    'echo', 'hi', ';', '&&', '||', '|', '&', '\n', ' ',
    '(', ')', '{', '}', '[', ']', '$', '"', "'", '`', '\\',
    '$(', '${', '$((', '))', '}', '#', '!', '~', '*', '?',
    'if', 'then', 'else', 'elif', 'fi', 'for', 'in', 'do', 'done',
    'while', 'until', 'case', 'esac', ';;', 'function', 'return',
    '<', '>', '>>', '<<', '<<-', '<&', '>&', '<>', '>|', '2>', '&1',
    'EOF', 'x=1', '$x', '"$@"', '$*', '$?', '$$', '$!', '$0', '$1',
    'a b c', '-n', '--', '=', ':', '.', ',', '+', '-', '/', '%',
]

SEED_CORPUS = [
    'echo hi',
    'if true; then echo a; else echo b; fi',
    'for i in 1 2 3; do echo $i; done',
    'while read l; do echo "$l"; done <<EOF\na\nb\nEOF',
    'case $x in a) echo a;; b|c) echo bc;; *) echo z;; esac',
    'f() { echo "$1"; }; f arg',
    'x=$(echo nested $(echo deep)); echo "${x:-def}"',
    'echo $((1 + 2 * (3 - 4) / 5 % 6))',
    'cat <<-EOF\n\tindented\n\tEOF',
    'a=1 b=2 env | grep x >out 2>&1',
    '(cd /tmp && ls) | while read f; do :; done',
    'echo "a\'b" \'c"d\' `echo e` $(echo f)',
    'trap "echo x" INT TERM; kill -TERM $$',
    'echo ${x#*.} ${y%%.*} ${z:2:3} ${#w}',
    '! true && false || :',
    'exec 3>&1; echo x >&3; exec 3>&-',
    "echo $'a\\tb'",
    'case x in [[:alpha:]]) echo y;; esac',
]


def harvest_corpus():
    """Pull additional seeds from the repo's own suites and the spec corpus."""
    out = list(SEED_CORPUS)
    spec = os.path.join(ROOT, 'third_party', 'oils', 'spec')
    if os.path.isdir(spec):
        for name in sorted(os.listdir(spec))[:40]:
            if not name.endswith('.test.sh'):
                continue
            try:
                text = open(os.path.join(spec, name), errors='replace').read()
            except OSError:
                continue
            # each #### block is a self-contained snippet
            for block in text.split('####')[1:]:
                body = block.split('\n##')[0]
                body = '\n'.join(body.splitlines()[1:]).strip()
                if body and len(body) < 400:
                    out.append(body)
    return out


def mutate(rng, s):
    """Apply one random mutation."""
    if not s:
        return rng.choice(FRAGMENTS)
    kind = rng.randrange(9)
    i = rng.randrange(len(s))
    if kind == 0:                                   # delete a span
        j = min(len(s), i + rng.randint(1, 8))
        return s[:i] + s[j:]
    if kind == 1:                                   # insert a fragment
        return s[:i] + rng.choice(FRAGMENTS) + s[i:]
    if kind == 2:                                   # duplicate a span
        j = min(len(s), i + rng.randint(1, 20))
        return s[:j] + s[i:j] + s[j:]
    if kind == 3:                                   # flip a byte
        return s[:i] + chr(rng.randrange(32, 127)) + s[i + 1:]
    if kind == 4:                                   # truncate
        return s[:i]
    if kind == 5:                                   # unbalance quoting
        return s[:i] + rng.choice(['"', "'", '`', '\\', '${', '$((']) + s[i:]
    if kind == 6:                                   # nest deeply
        n = rng.randint(2, 40)
        open_, close = rng.choice([('(', ')'), ('${', '}'), ('$((', '))'),
                                   ('{ ', '; }'), ('`', '`')])
        return open_ * n + s + close * n
    if kind == 7:                                   # splice two fragments
        return s[:i] + ' ' + rng.choice(FRAGMENTS) + ' ' + s[i:]
    return s + rng.choice(FRAGMENTS)                # append


def random_soup(rng):
    """Pure random token soup -- no valid seed involved."""
    n = rng.randint(1, 60)
    return ''.join(rng.choice(FRAGMENTS) for _ in range(n))


SANDBOX = {'dir': None}


def run(shell, src, timeout, execute):
    """Return (verdict, detail). Verdict is 'ok', 'crash', 'hang' or 'signal'."""
    argv = [shell] + ([] if execute else ['-n']) + ['-c', src]
    kw = {}
    if execute:
        # Reaching the expansion code means actually running commands, so bound
        # the blast radius: an empty PATH means no external command resolves,
        # and a throwaway cwd absorbs any redirection the input creates.
        kw['cwd'] = SANDBOX['dir']
        kw['env'] = {'PATH': '', 'HOME': SANDBOX['dir'], 'IFS': ' \t\n'}
    try:
        p = subprocess.run(argv, capture_output=True, timeout=timeout,
                           text=True, errors='replace', **kw)
    except subprocess.TimeoutExpired:
        return 'hang', 'no exit within %gs' % timeout
    except OSError as e:
        return 'crash', 'could not run: %s' % e
    combined = (p.stdout or '') + (p.stderr or '')
    if p.returncode is not None and p.returncode < 0:
        return 'signal', 'killed by signal %d' % -p.returncode
    m = CRASH_RE.search(combined)
    if m:
        line = next((l for l in combined.splitlines() if CRASH_RE.search(l)), '')
        return 'crash', line.strip()[:200]
    # Exit status alone proves nothing when executing: 126 and 127 are the
    # shell correctly reporting that a random word is not a command. Only
    # under -n, where nothing runs, does a status above 2 mean the parser
    # itself fell over.
    if not execute and p.returncode > 2:
        return 'crash', 'exit status %d: %s' % (
            p.returncode, combined.strip().splitlines()[:1])
    return 'ok', ''


def shrink(shell, src, timeout, execute, verdict):
    """Reduce SRC while it keeps failing the same way."""
    best = src
    changed = True
    while changed and len(best) > 1:
        changed = False
        # try removing halves, then progressively smaller chunks
        for size in (len(best) // 2, len(best) // 4, 8, 1):
            if size < 1:
                continue
            i = 0
            while i < len(best):
                cand = best[:i] + best[i + size:]
                if cand and run(shell, cand, timeout, execute)[0] == verdict:
                    best = cand
                    changed = True
                else:
                    i += size
    return best


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument('-n', '--iterations', type=int, default=2000)
    ap.add_argument('-s', '--seed', type=int, default=None)
    ap.add_argument('--shell', default=DEFAULT_SHELL)
    ap.add_argument('--timeout', type=float, default=5.0)
    ap.add_argument('--exec', dest='execute', action='store_true',
                    help='actually execute (DANGEROUS: sandbox only)')
    args = ap.parse_args()

    if not os.access(args.shell, os.X_OK):
        sys.exit('fuzz-parser: no executable at %s (run: make build)' % args.shell)

    if args.execute:
        import tempfile
        SANDBOX['dir'] = tempfile.mkdtemp(prefix='sxsh-fuzz-')

    seed = args.seed if args.seed is not None else random.randrange(1 << 30)
    rng = random.Random(seed)
    corpus = harvest_corpus()
    print('fuzz-parser: seed=%d iterations=%d corpus=%d %s'
          % (seed, args.iterations, len(corpus),
             ('(EXECUTING in %s, PATH empty)' % SANDBOX['dir'])
             if args.execute else '(parse only)'))

    findings = {}
    for i in range(args.iterations):
        if rng.randrange(4) == 0:
            src = random_soup(rng)
        else:
            src = rng.choice(corpus)
            for _ in range(rng.randint(1, 4)):
                src = mutate(rng, src)
        verdict, detail = run(args.shell, src, args.timeout, args.execute)
        if verdict != 'ok':
            small = shrink(args.shell, src, args.timeout, args.execute, verdict)
            key = (verdict, detail)
            if key not in findings:
                findings[key] = small
                print('\n%s: %s\n  input: %r' % (verdict.upper(), detail, small))

    print('\nfuzz-parser: %d iterations, %d distinct finding(s), seed=%d'
          % (args.iterations, len(findings), seed))
    return 1 if findings else 0


if __name__ == '__main__':
    sys.exit(main())

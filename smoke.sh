#!/usr/bin/env bash
# smoke.sh --- end-to-end check of the built `posh` binary.
#
# The ASDF suites (asdf:test-system "posh" / "posh/shell") exercise the parser
# and executor in-image. This script exercises what they cannot: the saved
# executable, argv handling in main, real process exit status, script mode, and
# stdin-driven interactive mode.
#
#   ./smoke.sh [path-to-posh]        (default: ./posh)

set -u

POSH=${1:-./posh}
pass=0
fail=0
tmp=$(mktemp -d "${TMPDIR:-/tmp}/posh-smoke.XXXXXX")
trap 'rm -rf "$tmp"' EXIT

# check <name> <expected-stdout> <expected-status> <shell-source>
check() {
  local name=$1 want=$2 want_st=$3 src=$4 got got_st
  got=$("$POSH" -c "$src" 2>"$tmp/err")
  got_st=$?
  if [ "$got" = "$want" ] && [ "$got_st" -eq "$want_st" ]; then
    pass=$((pass + 1))
  else
    fail=$((fail + 1))
    printf 'FAIL %s\n  src:    %s\n  want:   %q (status %d)\n  got:    %q (status %d)\n' \
      "$name" "$src" "$want" "$want_st" "$got" "$got_st"
    [ -s "$tmp/err" ] && printf '  stderr: %s\n' "$(cat "$tmp/err")"
  fi
}

# check_err <name> <expected-stderr-substring> <expected-status> <shell-source>
# For diagnostics the shell itself emits. Some of those are reported only after
# the failing construct's redirections have unwound, so they cannot be captured
# from inside the script under test -- we read the process's stderr instead.
check_err() {
  local name=$1 want=$2 want_st=$3 src=$4 err err_st
  err=$("$POSH" -c "$src" 2>&1 >/dev/null)
  err_st=$?
  case "$err" in
    *"$want"*)
      if [ "$err_st" -eq "$want_st" ]; then
        pass=$((pass + 1))
      else
        fail=$((fail + 1))
        printf 'FAIL %s\n  src:  %s\n  want status %d, got %d (stderr: %s)\n' \
          "$name" "$src" "$want_st" "$err_st" "$err"
      fi
      ;;
    *)
      fail=$((fail + 1))
      printf 'FAIL %s\n  src:    %s\n  want stderr containing: %s\n  got:    %s\n' \
        "$name" "$src" "$want" "$err"
      ;;
  esac
}

if [ ! -x "$POSH" ]; then
  echo "smoke: no executable at $POSH (run: make build)" >&2
  exit 1
fi

# --- core execution -------------------------------------------------------
check builtin-echo      'hi'        0 'echo hi'
check external-spawn    'hi'        0 '/bin/echo hi'
check path-lookup       'hi'        0 'echo hi'
check exit-status       ''          7 'exit 7'
check false-status      ''          1 'false'
check not-found         ''        127 'definitely-not-a-real-command-xyz'

# --- pipelines ------------------------------------------------------------
check pipeline-2        'b'         0 'printf "a\nb\n" | tail -1'
check pipeline-3        '1'         0 'printf "3\n1\n2\n" | sort | head -1'
check pipeline-status   ''          1 'true | false'
check pipeline-bang     ''          0 '! false'

# --- redirection ----------------------------------------------------------
check redirect-out      'x'         0 "echo x > $tmp/a; cat $tmp/a"
check redirect-append   'x
y'                                  0 "echo x > $tmp/b; echo y >> $tmp/b; cat $tmp/b"
check redirect-in       'z'         0 "echo z > $tmp/c; cat < $tmp/c"
check redirect-fd       ''          0 'echo err >&2'
check heredoc           'one
two'                                0 'cat <<EOF
one
two
EOF'
check heredoc-quoted    '$notexpanded' 0 "cat <<'EOF'
\$notexpanded
EOF"

# --- expansion ------------------------------------------------------------
check var-assign        '7'         0 'X=7; echo $X'
check param-default     'd'         0 'echo ${U:-d}'
check param-length      '4'         0 'V=abcd; echo ${#V}'
check param-suffix      'a.b|a'     0 'F=a.b.c; echo "${F%.*}|${F%%.*}"'
check cmd-substitution  '[inner]'   0 'echo [$(echo inner)]'
check backquote         'inner'     0 'echo `echo inner`'
check arithmetic        '13'        0 'echo $((2**0 + 6 / 2 * 4))'
check field-splitting   'a b c'     0 'S="a b c"; echo $S'
check quoted-star       'a b c'     0 'set -- a b c; echo "$*"'

# --- control flow ---------------------------------------------------------
check if-then           'eq'        0 'if [ 1 -eq 1 ]; then echo eq; fi'
check for-loop          'x
y'                                  0 'for i in x y; do echo $i; done'
check while-loop        '0
1'                                  0 'n=0; while [ $n -lt 2 ]; do echo $n; n=$((n+1)); done'
check case-clause       'matched'   0 'case abc in a*) echo matched;; esac'
check and-or            'ok'        0 'true && echo ok'
check or-short-circuit  'ok'        0 'false || echo ok'
check function-def      'called'    0 'f() { echo called; }; f'
check function-args     'arg1'      0 'f() { echo $1; }; f arg1'

# --- state isolation ------------------------------------------------------
check subshell-isolates 'outer'     0 'V=outer; (V=inner); echo $V'
check subshell-cd       'same'      0 'D=$(pwd); (cd /); [ "$(pwd)" = "$D" ] && echo same'

# --- jobs & traps ---------------------------------------------------------
check background-wait   'done'      0 'sleep 0.1 & wait; echo done'
check trap-exit         'bye'       0 'trap "echo bye" EXIT'
check set-m-on          '0'         0 'set -m; case $- in *m*) echo 0;; *) echo no;; esac'
check set-m-off         '0'         0 'set +m; case $- in *m*) echo no;; *) echo 0;; esac'

# --- asynchronous compounds (re-exec, not synchronous fallback) -----------
# If the compound ran synchronously we would see LATE before FIRST.
check async-order       'FIRST
LATE'                               0 '{ sleep 0.4; echo LATE; } & echo FIRST; wait'
check async-bang-pid    'yes'       0 '{ sleep 0.1; } & case $! in [0-9]*) echo yes;; *) echo "no:$!";; esac; wait'
check async-function    'main
from-func'                          0 'f() { sleep 0.3; echo from-func; }; f & echo main; wait'
check async-inherits-var 'got=secret' 0 'V=secret; { echo "got=$V"; } & wait'
check async-inherits-pos 'n=3 a b c'  0 'set -- a b c; { echo "n=$# $*"; } & wait'
check async-isolated    'outer'     0 'V=outer; { V=inner; } & wait; echo $V'

# --- set -e scoping (POSIX exempts condition contexts) --------------------
check errexit-plain      ''          1 'set -e; false; echo S'
check errexit-andand-1st 'S'         0 'set -e; false && true; echo S'
check errexit-andand-lst ''          1 'set -e; true && false; echo S'
check errexit-oror-last  ''          1 'set -e; false || false; echo S'
check errexit-chain      'S'         0 'set -e; true && false && true; echo S'
check errexit-if-cond    'S'         0 'set -e; if false; then :; fi; echo S'
check errexit-if-body    ''          1 'set -e; if true; then false; fi; echo S'
check errexit-while-cond 'S'         0 'set -e; while false; do :; done; echo S'
check errexit-until-cond 'S'         0 'set -e; until true; do :; done; echo S'
check errexit-bang       'S'         0 'set -e; ! true; echo S'
check errexit-pipe-early 'S'         0 'set -e; false | true; echo S'
check errexit-pipe-last  ''          1 'set -e; true | false; echo S'
check errexit-subshell   ''          1 'set -e; (false); echo S'
check errexit-brace      ''          1 'set -e; { false; }; echo S'
check errexit-function   ''          1 'set -e; f() { false; }; f; echo S'
check errexit-for-body   ''          1 'set -e; for i in 1; do false; done; echo S'

# --- read honours IFS -----------------------------------------------------
check read-ifs-colon    'a-b-c'      0 'echo "a:b:c" | { IFS=: read x y z; echo "$x-$y-$z"; }'
check read-ifs-remain   '[a][b:c:d]' 0 'echo "a:b:c:d" | { IFS=: read x y; echo "[$x][$y]"; }'
check read-ifs-empty    '[a][][b]'   0 'echo "a::b" | { IFS=: read x y z; echo "[$x][$y][$z]"; }'
check read-ifs-leading  '[][a]'      0 'echo ":a" | { IFS=: read x y; echo "[$x][$y]"; }'
check read-ws-trim      '[a][b]'     0 'echo "  a  b  " | { read x y; echo "[$x][$y]"; }'
check read-mixed-ifs    '[a][b][c:d]' 0 'echo "a:b c:d" | { IFS=": " read p q r; echo "[$p][$q][$r]"; }'

# --- kill builtin, including job specs ------------------------------------
check kill-jobspec      'killed'     0 'sleep 5 & kill %1 && echo killed'
check kill-named-sig    'killed'     0 'sleep 5 & kill -TERM %1 && echo killed'
check kill-s-form       'killed'     0 'sleep 5 & kill -s KILL %1 && echo killed'
check kill-by-pid       'killed'     0 'sleep 5 & kill -9 $! && echo killed'
check kill-l-number     'TERM'       0 'kill -l 15'
check kill-l-waitstatus 'TERM'       0 'kill -l 143'
check kill-is-builtin   'kill'       0 'command -v kill'
check kill-bad-signal   ''           1 'kill -NOSUCHSIG 1 2>/dev/null'
check kill-bad-job      ''           1 'kill %99 2>/dev/null'

# --- EXIT trap must not overwrite the exit status -------------------------
check trap-keeps-status 't'          3 'trap "echo t" EXIT; exit 3'
check trap-explicit-win ''           7 'trap "exit 7" EXIT; exit 3'
check trap-last-command 't'          1 'trap "echo t" EXIT; false'

# --- a command substitution in the words runs exactly once ----------------
# Count the substitution's side effects in a file: running it twice shows up
# as two lines. Asserting on stderr would not work -- the substitution runs in
# the shell, before the command's own redirections exist.
check cmdsub-once-ext   'hi
1'                                   0 'f=$(mktemp); /bin/echo hi$(echo x >>"$f"); grep -c x "$f"; rm -f "$f"'
check cmdsub-once-pipe  'hi
1'                                   0 'f=$(mktemp); /bin/echo hi$(echo x >>"$f") | cat; grep -c x "$f"; rm -f "$f"'
check cmdsub-once-async 'hi
1'                                   0 'f=$(mktemp); /bin/echo hi$(echo x >>"$f") & wait; grep -c x "$f"; rm -f "$f"'

# --- diagnostics honour the command's redirections ------------------------
check notfound-quiet    '127'        0 'nosuchcmd_xyz 2>/dev/null; echo $?'
check notfound-pipe     'st=127'     0 'echo hi | nosuchcmd_xyz 2>/dev/null; echo "st=$?"'
check notfound-nocrash  'st=0'       0 'nosuchcmd_xyz 2>/dev/null | cat; echo "st=$?"'

# --- arithmetic errors are shell diagnostics, not Lisp conditions ---------
check_err arith-div0    'division by 0' 1 'echo $((1/0))'
check_err arith-mod0    'division by 0' 1 'echo $((5%0))'
check_err arith-div0-assign 'division by 0' 1 'x=1; echo $((x/=0))'
check_err notfound-msg  'command not found' 127 'nosuchcmd_xyz'

# --- export -p listing ----------------------------------------------------
check export-p          "export FOO='1'" 0 'export FOO=1; export -p | grep "^export FOO="'

# --- aliases expand in non-interactive shells (bash deviates; POSIX does not)
check alias-expands     'aliased'    0 'alias g="echo aliased"
g'

# --- pathname expansion applies to unquoted expansion results (POSIX 2.6) --
check glob-from-var     'g1 g2'      0 'cd "$(mktemp -d)"; touch g1 g2; p=g*; echo $p'
check glob-from-quoted-assign 'g1 g2' 0 'cd "$(mktemp -d)"; touch g1 g2; p="g*"; echo $p'
check glob-from-cmdsub  'g1 g2'      0 'cd "$(mktemp -d)"; touch g1 g2; echo $(echo "g*")'
check glob-from-params  'g1 g2'      0 'cd "$(mktemp -d)"; touch g1 g2; set -- "g*"; echo $@'
check glob-quoted-stays 'g*'         0 'cd "$(mktemp -d)"; touch g1 g2; p=g*; echo "$p"'
check glob-literal-quoted 'g*'       0 'cd "$(mktemp -d)"; touch g1 g2; echo "g*"'
check glob-noglob-opt   'g*'         0 'cd "$(mktemp -d)"; touch g1 g2; set -f; p=g*; echo $p'
check glob-nomatch-literal 'zz*'     0 'cd "$(mktemp -d)"; p=zz*; echo $p'

# --- POSIX bracket expressions --------------------------------------------
check class-alpha       'y'          0 'case a in [[:alpha:]]) echo y;; *) echo n;; esac'
check class-digit       'y'          0 'case 5 in [[:digit:]]) echo y;; *) echo n;; esac'
check class-upper-neg   'n'          0 'case a in [[:upper:]]) echo y;; *) echo n;; esac'
check class-space       'y'          0 'case " " in [[:space:]]) echo y;; *) echo n;; esac'
check class-xdigit      'y'          0 'case f in [[:xdigit:]]) echo y;; *) echo n;; esac'
check class-negated     'y'          0 'case 5 in [![:alpha:]]) echo y;; *) echo n;; esac'
check class-mixed-set   'y'          0 'case 7 in [abc[:digit:]]) echo y;; *) echo n;; esac'
check class-equiv       'y'          0 'case a in [[=a=]]) echo y;; *) echo n;; esac'
check class-collate     'y'          0 'case a in [[.a.]]) echo y;; *) echo n;; esac'
check class-range-still 'y'          0 'case c in [a-z]) echo y;; *) echo n;; esac'

# --- set options ----------------------------------------------------------
check set-o-lists       'yes'        0 'set -o | grep -q "^noclobber" && echo yes'
check set-o-plus-form   'yes'        0 'set +o | grep -q "^set [+-]o errexit" && echo yes'
check set-o-long-name   'yes'        0 'set -o noglob; case $- in *f*) echo yes;; esac'
check set-noclobber     'a'          0 'cd "$(mktemp -d)"; set -C; echo a>f; echo b>f 2>/dev/null; cat f'
check set-noclobber-st  '1'          0 'cd "$(mktemp -d)"; set -C; echo a>f; echo b>f 2>/dev/null; echo $?'
check set-noclobber-force 'b'        0 'cd "$(mktemp -d)"; set -C; echo a>f; echo b>|f; cat f'
check set-allexport     '1'          0 'set -a; V=x; env | grep -c "^V=x"'
check set-noexec        ''           0 'set -n; echo NOTRUN'
check set-invalid-opt   ''           2 'set -Z 2>/dev/null'

# --- read: escapes and EOF status (POSIX 2.14) ----------------------------
check read-eof-status   'st=1 x=a'   0 'printf "a" | { read x; echo "st=$? x=$x"; }'
check read-newline-st   'st=0 x=a'   0 'printf "a\n" | { read x; echo "st=$? x=$x"; }'
check read-backslash    '[ab]'       0 'printf "a\\\\b\n" | { read x; echo "[$x]"; }'
check read-raw-keeps    '[a\b]'      0 'printf "a\\\\b\n" | { read -r x; echo "[$x]"; }'
check read-escaped-ifs  '[a b][]'    0 'printf "a\\\\ b\n" | { read x y; echo "[$x][$y]"; }'
check read-escaped-delim '[a:b][]'   0 'printf "a\\\\:b\n" | { IFS=: read x y; echo "[$x][$y]"; }'
check read-continuation '[ab]'       0 'printf "a\\\\\nb\n" | { read x; echo "[$x]"; }'
check read-while-no-nl  'got:l1'     0 'printf "l1\nl2" | { while read l; do echo "got:$l"; done; }'

# --- ${x:?} and set -u terminate a non-interactive shell ------------------
# POSIX requires the shell to exit with a non-zero status; the value itself is
# unspecified (bash uses 127, we and zsh use 1), so only the exit is asserted.
check colon-question-exits ''    1 'echo ${x:?} 2>/dev/null; echo AFTER'
check colon-question-msg   ''    1 'echo ${x:?msg} 2>/dev/null; echo AFTER'
check nounset-exits        ''    1 'set -u; echo $u 2>/dev/null; echo AFTER'
check_err colon-question-text 'parameter null or not set' 1 'echo ${x:?}'
check_err colon-question-custom 'msg' 1 'echo ${x:?msg}'
check colon-question-set   'v'   0 'x=v; echo ${x:?}'

# --- alias substitution (POSIX 2.3.1) -------------------------------------
# bash cannot be the reference here: it disables aliases in non-interactive
# shells, its own documented deviation. Verified against zsh instead.
check alias-basic       'hi'     0 'alias g="echo hi"
g'
check alias-trailing-blank 'world' 0 'alias g="echo "
alias h=world
g h'
check alias-blank-extra 'world extra' 0 'alias g="echo "
alias h=world
g h extra'
check alias-no-blank    'hi world' 0 'alias g="echo hi"
g world'
check alias-chained     'A'      0 'alias a="echo A"
alias b=a
b'
check alias-listing     "alias g='echo x'" 0 'alias g="echo x"; alias g'
check alias-unalias     '127'    0 'alias g="echo hi"; unalias g; g 2>/dev/null; echo $?'
check alias-unalias-all ''       0 'alias g=x; unalias -a; alias'

# --- redirections apply to compound commands ------------------------------
check heredoc-while     '[a]
[b]'                            0 'while read l; do echo "[$l]"; done <<EOF
a
b
EOF'
check heredoc-for       '[y]'   0 'for i in 1; do read l; echo "[$l]"; done <<EOF
y
EOF'
check heredoc-if        '[z]'   0 'if read l; then echo "[$l]"; fi <<EOF
z
EOF'
check heredoc-case      '[c]'   0 'case x in x) read l; echo "[$l]";; esac <<EOF
c
EOF'
check heredoc-until     '[u]'   0 'until read l; do :; done <<EOF
u
EOF
echo "[$l]"'
check redirect-for-out  'a
b'                              0 'f=$(mktemp); for i in a b; do echo $i; done > "$f"; cat "$f"; rm -f "$f"'
check redirect-while-in '2'     0 'f=$(mktemp); printf "x\ny\n" > "$f"; n=0; while read l; do n=$((n+1)); done < "$f"; echo $n; rm -f "$f"'

# --- redirection failures name the file and fail only the command ---------
check_err redir-missing-in 'No such file or directory' 1 'cat < /nonexistent-zz'
check redir-missing-status '1' 0 'cat < /nonexistent-zz 2>/dev/null; echo $?'
check redir-shell-survives 'after' 0 'cat < /nonexistent-zz 2>/dev/null; echo after'
check_err redir-bad-outdir 'No such file or directory' 1 'echo x > /nonexistent-dir-zz/f'
check notfound-still-127  '127' 0 'nosuchcmd_zz 2>/dev/null; echo $?'

# --- trap listing order and subshell trap isolation -----------------------
check trap-order        "trap -- 'echo t' EXIT
trap -- 'echo t' SIGINT
trap -- 'echo t' SIGTERM
t"                              0 'trap "echo t" EXIT INT TERM; trap'
check trap-order-stable "trap -- 'echo b' SIGINT
trap -- 'echo a' SIGTERM" 0 'trap "echo a" TERM; trap "echo b" INT; trap'
check trap-subshell-own 'in
S
after'                          0 "(trap 'echo S' EXIT; echo in); echo after"
check trap-parent-once  'sub
after
P'                              0 "trap 'echo P' EXIT; (echo sub); echo after"
check trap-cmdsub-once  'cs
after
P'                              0 "trap 'echo P' EXIT; echo \$(echo cs); echo after"
check trap-no-leak      ''      0 "(trap 'echo S' INT; :); trap"

# --- quoted metacharacters stay inert in patterns --------------------------
check pat-escaped-star-no  'glob' 0 'case "axb" in a\*b) echo lit;; *) echo glob;; esac'
check pat-escaped-star-yes 'lit'  0 'case "a*b" in a\*b) echo lit;; *) echo glob;; esac'
check pat-quoted-star      'lit'  0 'case "a*b" in "a*b") echo lit;; *) echo glob;; esac'
check pat-escaped-dash     'y'    0 'case "-" in [a\-z]) echo y;; *) echo n;; esac'
check pat-escaped-dash-neg 'n'    0 'case "b" in [a\-z]) echo y;; *) echo n;; esac'
check pat-var-globs        'glob' 0 'v="a*b"; case "axb" in $v) echo glob;; *) echo lit;; esac'
check pat-plain-star       'y'    0 'case abc in a*) echo y;; esac'
check pat-bracket-rbracket 'y'    0 'case "]" in []]) echo y;; *) echo n;; esac'

# --- cd option and path validation ----------------------------------------
check cd-double-dash    '/tmp'  0 'cd -- /tmp; pwd'
check cd-bad-component  ''      1 'cd /tmp/NOPE_xyz/.. 2>/dev/null'
check pwd-double-dash   'yes'   0 'cd /tmp; [ "$(pwd --)" = /tmp ] && echo yes'

# --- previously-known bugs: command -p, . PATH search, CDPATH, vars -------
check command-p-runs    'hi'    0 'command -p echo hi'
check command-p-stdpath '0'     0 'PATH=/nonexistent command -p true; echo $?'
check command-plain     'hi'    0 'command echo hi'
check command-v         'echo'  0 'command -v echo'
check command-bypass-fn '127'   0 'f(){ echo func; }; command f 2>/dev/null; echo $?'
check dot-path-search   'sourced-ok' 0 'd=$(mktemp -d); echo "echo sourced-ok" > "$d/lib.sh"; PATH=$d:$PATH; . lib.sh'
check dot-sets-vars     '[v]'   0 'd=$(mktemp -d); echo "V=v" > "$d/lib.sh"; PATH=$d:$PATH; . lib.sh; echo "[$V]"'
check dot-absolute      'sourced-ok' 0 'd=$(mktemp -d); echo "echo sourced-ok" > "$d/lib.sh"; . "$d/lib.sh"'
check dot-missing       ''      1 '. /nonexistent/nope.sh 2>/dev/null'
check cdpath-finds      'yes'   0 'd=$(mktemp -d); mkdir "$d/proj"; CDPATH=$d; cd proj >/dev/null; case "$PWD" in */proj) echo yes;; esac'
check cdpath-absolute   '/tmp'  0 'd=$(mktemp -d); mkdir "$d/proj"; CDPATH=$d; cd /tmp; pwd'
check optind-initial    '1'     0 'echo $OPTIND'
check ppid-set          'yes'   0 'test "$PPID" -gt 0 && echo yes'
check lineno-tracks     '1
2'                              0 'echo $LINENO
echo $LINENO'

# --- trap forms (POSIX 2.14) ----------------------------------------------
check trap-multi-signal "trap -- 'echo hi' SIGINT
trap -- 'echo hi' SIGTERM" 0 'trap "echo hi" INT TERM; trap'
check trap-full-names   "trap -- 'echo hi' SIGINT" 0 'trap "echo hi" INT; trap'
check trap-reset-bare   'done'  0 'trap "echo e" EXIT; trap EXIT; echo done'
check trap-reset-zero   'done'  0 'trap "echo e" EXIT; trap 0; echo done'
check trap-zero-is-exit 'done
e'                              0 'trap "echo e" 0; echo done'
check trap-dash-removes ''      0 'trap "echo hi" INT; trap - INT; trap'
check trap-p-one        "trap -- 'echo hi' SIGINT" 0 'trap "echo hi" INT; trap -p INT'
check trap-double-dash  "trap -- 'echo hi' SIGINT" 0 'trap -- "echo hi" INT; trap'
check trap-ignore-empty "trap -- '' SIGINT" 0 "trap '' INT; trap"
check trap-ignore-works 'survived' 0 "trap '' INT; kill -INT \$\$; echo survived"
check trap-bad-signal   ''      1 'trap "echo x" NOSUCHSIG 2>/dev/null'

# --- temp assignments and declaration operands ----------------------------
check tempenv-sees-prev '1'     0 'a=1 b=$a env 2>/dev/null | grep -c "^b=1"'
check tempenv-no-leak   'outer' 0 'x=outer; x=inner env >/dev/null; echo $x'
check export-no-split   '[a b]' 0 'v="a b"; export x=$v; echo "[$x]"'
check readonly-no-split '[a b]' 0 'v="a b"; readonly x=$v; echo "[$x]"'
check export-no-glob    '*.c'   0 'cd "$(mktemp -d)"; touch q.c; export x=*.c; echo "$x"'
check noclobber-devnull '0'     0 'set -C; echo hi > /dev/null; echo $?'

# --- arithmetic: increment/decrement and integer-only errors --------------
check arith-postincr    '1
2'                              0 'x=1; echo $((x++)); echo $x'
check arith-preincr     '2
2'                              0 'x=1; echo $((++x)); echo $x'
check arith-postdecr    '5
4'                              0 'x=5; echo $((x--)); echo $x'
check arith-predecr     '4
4'                              0 'x=5; echo $((--x)); echo $x'
check arith-incr-unset  '0
1'                              0 'echo $((y++)); echo $y'
check arith-compound    '3
3'                              0 'x=1; echo $((x+=2)); echo $x'
check_err arith-neg-exponent 'exponent less than 0' 1 'echo $((2**-1))'
check_err arith-float   'invalid arithmetic operator' 1 'echo $((1.5))'
check_err arith-nounset 'parameter not set'    1 'set -u; echo $((undef))'
check arith-exp-ok      '8'     0 'echo $((2**3))'

# --- tilde expansion (POSIX 2.6.1) ----------------------------------------
check tilde-word        'yes'   0 '[ "$(echo ~)" = "$HOME" ] && echo yes'
check tilde-assign      'yes'   0 'x=~; [ "$x" = "$HOME" ] && echo yes'
check tilde-after-colon 'yes'   0 'x=a:~; [ "$x" = "a:$HOME" ] && echo yes'
check tilde-export      'yes'   0 'export x=~; [ "$x" = "$HOME" ] && echo yes'
check tilde-readonly    'yes'   0 'readonly x=~; [ "$x" = "$HOME" ] && echo yes'
check tilde-export-path 'yes'   0 'export p=a:~:b; [ "$p" = "a:$HOME:b" ] && echo yes'
check tilde-quoted      '~'     0 'echo "~"'
check tilde-default-sub 'yes'   0 '[ "$(echo ${undef:-~})" = "$HOME" ] && echo yes'
# zsh and POSIX agree that a plain operand is NOT an assignment context; bash
# expands here as an extension, so this deliberately differs from bash.
check tilde-plain-operand 'a=~' 0 'echo a=~'

# --- IFS field splitting in expansions (POSIX 2.6.5) ----------------------
check ifs-unset-default 2       0 'unset IFS; v="a b"; set -- $v; echo $#'
check ifs-unset-star    'a b'   0 'unset IFS; set -- a b; echo "$*"'
check ifs-empty-nosplit 1       0 'IFS=; v="a b"; set -- $v; echo $#'
check ifs-ws-delim-run  2       0 'v="a : b"; IFS=" :"; set -- $v; echo $#'
check ifs-ws-delim-run2 2       0 'v="a  :  b"; IFS=" :"; set -- $v; echo $#'
check ifs-empty-field   3       0 'v="a::b"; IFS=:; set -- $v; echo $#'
check ifs-leading-delim 2       0 'v=":a"; IFS=:; set -- $v; echo $#'
check ifs-trailing-delim 1      0 'v="a:"; IFS=:; set -- $v; echo $#'
check ifs-all-ws        0       0 'v="  "; set -- $v; echo $#'
check ifs-ws-trim       '2|[a][b]' 0 'v="  a  b  "; set -- $v; echo "$#|[$1][$2]"'
check ifs-unset-read    '[a][b]' 0 'unset IFS; printf "a\tb\n" | { read x y; echo "[$x][$y]"; }'

# --- logical working directory (POSIX cd/pwd -L and -P) -------------------
check pwd-logical       '/tmp/pl/link' 0 'rm -rf /tmp/pl; mkdir -p /tmp/pl/real; ln -s real /tmp/pl/link; cd /tmp/pl/link; pwd'
check pwd-physical-opt  'yes'   0 'rm -rf /tmp/pl; mkdir -p /tmp/pl/real; ln -s real /tmp/pl/link; cd /tmp/pl/link; case "$(pwd -P)" in */pl/real) echo yes;; esac'
check pwd-var-matches   'same'  0 'rm -rf /tmp/pl; mkdir -p /tmp/pl/real; ln -s real /tmp/pl/link; cd /tmp/pl/link; [ "$PWD" = "$(pwd)" ] && echo same'
check cd-dotdot-logical '/tmp/pl' 0 'rm -rf /tmp/pl; mkdir -p /tmp/pl/real; ln -s real /tmp/pl/link; cd /tmp/pl/link; cd ..; pwd'
check cd-P-resolves     'yes'   0 'rm -rf /tmp/pl; mkdir -p /tmp/pl/real; ln -s real /tmp/pl/link; cd -P /tmp/pl/link; case "$(pwd)" in */pl/real) echo yes;; esac'
check pwd-ignores-lie   'yes'   0 'cd /tmp; PWD=/nonexistent-lie; case "$(pwd)" in */tmp) echo yes;; esac'
check cd-too-many-args  ''      2 'cd a b 2>/dev/null'
check cd-oldpwd         '/tmp'  0 'cd /tmp; cd /; cd - >/dev/null; pwd'

# --- working directory ----------------------------------------------------
check cd-pwd            '/usr'      0 'cd /usr; pwd'
check cd-updates-PWD    '/usr'      0 'cd /usr; echo $PWD'
check cd-dash           '/usr'      0 'cd /usr; cd /etc; cd - >/dev/null; pwd'
check cd-subshell-scope '/usr'      0 'cd /usr; (cd /etc); pwd'
check cd-external-agrees 'same'     0 'cd /usr; [ "$(pwd)" = "$(/bin/pwd)" ] && echo same'

# --- script mode (not -c) -------------------------------------------------
printf '#!/usr/bin/env posh\necho script-mode\necho "$1"\n' > "$tmp/s.sh"
got=$("$POSH" "$tmp/s.sh" firstarg 2>&1); got_st=$?
if [ "$got" = "script-mode
firstarg" ] && [ "$got_st" -eq 0 ]; then
  pass=$((pass + 1))
else
  fail=$((fail + 1))
  printf 'FAIL script-mode\n  want: %q\n  got:  %q (status %d)\n' \
    "script-mode
firstarg" "$got" "$got_st"
fi

# --- stdin / REPL mode ----------------------------------------------------
got=$(printf 'echo from-stdin\nexit 0\n' | "$POSH" 2>/dev/null)
if printf '%s' "$got" | grep -q 'from-stdin'; then
  pass=$((pass + 1))
else
  fail=$((fail + 1))
  printf 'FAIL stdin-mode\n  want output containing from-stdin\n  got: %q\n' "$got"
fi

printf '\nsmoke: %d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]

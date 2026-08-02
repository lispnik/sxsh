#!/usr/bin/env bash
# smoke.sh --- end-to-end check of the built `sxsh` binary.
#
# The ASDF suites (asdf:test-system "sxsh" / "sxsh/shell") exercise the parser
# and executor in-image. This script exercises what they cannot: the saved
# executable, argv handling in main, real process exit status, script mode, and
# stdin-driven interactive mode.
#
#   ./smoke.sh [path-to-sxsh]        (default: ./sxsh)

set -u

SXSH=${1:-./bin/sxsh}
# Absolute: some cases cd into a scratch directory to exercise descriptor
# redirections, and a relative path would stop resolving there.
case $SXSH in /*) ;; *) SXSH=$PWD/${SXSH#./} ;; esac
pass=0
fail=0
tmp=$(mktemp -d "${TMPDIR:-/tmp}/sxsh-smoke.XXXXXX")
trap 'rm -rf "$tmp"' EXIT
# Exported so a test's shell source can write scratch files of its own; the
# unexported `tmp' is not visible to the shell under test.
export SMOKE_TMP="$tmp"

# check <name> <expected-stdout> <expected-status> <shell-source>
check() {
  local name=$1 want=$2 want_st=$3 src=$4 got got_st
  got=$("$SXSH" -c "$src" 2>"$tmp/err")
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
  err=$("$SXSH" -c "$src" 2>&1 >/dev/null)
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

if [ ! -x "$SXSH" ]; then
  echo "smoke: no executable at $SXSH (run: make build)" >&2
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

# --- redirections: dup, move, close, {name} allocation ---------------------
# Expectations diffed against bash before being written down. These run in
# their own directory so the fd files cannot collide with other cases.
rdir="$tmp/redir"; mkdir -p "$rdir"
# Not a subshell: `check' increments the pass/fail counters, and a subshell
# would silently discard them -- failures printed while the total stayed 0.
rcheck() { local back=$PWD; cd "$rdir" || return; rm -f f.txt file1 out.txt
           check "$@"; cd "$back" || return; }
# `n>&n' succeeds even when n is closed; a DIFFERENT bad fd fails the command
# (and only the command -- the raw SB-POSIX error used to kill the shell).
rcheck redir-self-dup   'hello'  0 ': 3>&3; echo hello'
rcheck redir-bad-fd     'st=1'   0 'echo x >&7 2>/dev/null; echo st=$?'
rcheck redir-close      'hello'  0 'exec 5> f.txt; echo hello >&5; exec 5>&-; echo world >&5 2>/dev/null; cat f.txt'
rcheck redir-move       'hello5
world6'                          0 'exec 5> f.txt; echo hello5 >&5; exec 6>&5-; echo world5 >&5 2>/dev/null; echo world6 >&6; exec 6>&-; cat f.txt'
# A command-scoped fd is gone again by the next command, subshell included.
rcheck redir-not-leaked 'st=0'   0 'true 9> f.txt; ( echo world >&9 ) 2>/dev/null; cat f.txt; echo st=$?'
# `>&word' with a non-numeric word sends BOTH streams to the file...
rcheck redir-both       'STDERR
STDOUT'                          0 'f(){ echo STDOUT; echo STDERR >&2; }; f >&out.txt; sort out.txt'
# ...but only without an explicit descriptor.
rcheck redir-ambiguous  'st=1'   0 'echo hi 2>&out.txt 2>/dev/null; echo st=$?'
# `{name}>' allocates a fresh descriptor >= 10 and stores its number.
rcheck redir-varfd      'named-fd-contents' 0 'exec {myfd}> f.txt; echo named-fd-contents >&$myfd; cat f.txt'
rcheck redir-varfd-close 'foo55' 0 'exec {fd}>file1; echo foo55 >&$fd; exec {fd}>&-; echo bar >&$fd 2>/dev/null; cat file1'
# The empty first line is the point: `{myvar}>' takes a NEW descriptor, so
# echo's own stdout is untouched and it still prints its (empty) argument list.
rcheck redir-varfd-num  '
ge10=1'                          0 'echo {myvar}>/dev/null; echo "ge10=$((myvar>=10))"'
# `{name}' is a redirection only as a whole word immediately before < or >.
rcheck redir-varfd-word1 'x={myvar}' 0 'echo x={myvar}>/dev/stdout'
rcheck redir-varfd-word2 'x={myvar}' 0 'echo x={myvar} >/dev/stdout'
rcheck redir-varfd-word3 '+{myvar}'  0 'echo +{myvar}>/dev/stdout'
rcheck redir-varfd-word4 'x='        0 'echo x= {myvar}>/dev/stdout'
# The shell's own saved descriptors must sit outside the range a script names:
# closing fd 10 once freed it for the next command's stdout backup, so `>&10'
# silently duplicated stdout instead of failing.
rcheck redir-backup-range 'st=1
bar'                             0 'exec 10>file1; echo bar >&10; exec 10>&-; echo after >&10 2>/dev/null; echo st=$?; cat file1'
# `err' goes to STDERR -- a plain `check' captures stdout only and would pass
# on an empty string no matter where the text went.
rcheck_err() { local back=$PWD; cd "$rdir" || return; check_err "$@"; cd "$back" || return; }
rcheck_err redir-backup-safe 'err' 0 '(exec 3>&1) 2>/dev/null; echo err >&2'

# --- arithmetic: bases, constant validation, subscripts, short circuit -----
# Expectations diffed against bash before being written down.
# BASE#DIGITS: 0-9, a-z = 10..35, A-Z = 36..61, @ = 62, _ = 63. Case only
# carries information above base 36, so 36#A and 36#a are both 10.
check arith-base36      '10-35 10'  0 'echo $((36#a))-$((36#z)) $((36#A))'
check arith-base64      '10 35 36 61 62 63' 0 'echo $((64#a)) $((64#z)) $((64#A)) $((64#Z)) $((64#@)) $((64#_))'
check arith-base-multi  '123 27 6151' 0 'echo $((10#0123)) $((16#1b)) $((24#ag7))'
check arith-base-dyn    '10'        0 'base=16; echo $(( ${base}#a ))'
check arith-prefixes    '298 10 511 8 9' 0 'echo $((0x12A)) $((0x0A)) $((0777)) $((0010)) $((011))'
check arith-dyn-octal   '9'         0 'zero=0; echo $(( ${zero}11 ))'
# Malformed constants are errors, not a number followed by a name.
check_err arith-bad-hex   'invalid' 1 'echo $(( 0x1X ))'
check_err arith-bad-octal 'invalid' 1 'echo $(( 09 ))'
check_err arith-digit-base 'base'   1 'echo $(( 2#A ))'
check_err arith-zero-base 'base'    1 'echo $(( 02#0110 ))'
check_err arith-base-low  'base'    1 'echo $((1#0))'
check_err arith-base-high 'base'    1 'echo $((65#a))'
# Array subscripts are part of the arithmetic grammar.
check arith-subscript   '11'        0 'a=(1 2 3 4); echo $((a[1] + a[2]*3))'
check arith-scalar-sub  '42 0'      0 's=42; echo $((s[0])) $((s[1]))'
check arith-sub-assign  '12
1 12 3'                             0 'a=(1 2 3); echo $((a[1]+=10)); echo "${a[@]}"'
check arith-sub-incdec  '6 7 6 7'   0 'a=(5 6 7 8); (( a[0]++, ++a[1], a[2]--, --a[3] )); echo "${a[@]}"'
check arith-sub-undef   '1 1 -1 -1' 0 '(( undef[0]++, ++undef[1], undef[2]--, --undef[3] )); echo "${undef[@]}"'
check arith-dyn-name    '42
xbar[5]=42'                         0 'foo=bar; echo $(( x$foo[5] = 42 )); echo "xbar[5]=${xbar[5]}"'
check_err arith-double-sub 'syntax' 1 'a=(1 2 3); echo $((a[1][1]))'
# An out-of-range element of an array that EXISTS is 0; a subscript of a name
# that does not exist is still an unset error. (bash -c reports 127 for this
# where a bash script reports 1; run as a script the two agree.)
check arith-sub-range   '0'         0 'set -u; a=(1); echo $(( a[5] ))'
check_err arith-sub-unset 'not set' 1 'set -u; echo $(( undef[0] ))'
# Short circuit: the untaken operand must not take effect.
check arith-or-skips    '11'        0 'x=11; (( 1 || (x = 22) )); echo $x'
check arith-or-runs     '33'        0 'x=11; (( 0 || (x = 33) )); echo $x'
check arith-and-skips   '11'        0 'x=11; (( 0 && (x = 44) )); echo $x'
check arith-and-runs    '55'        0 'x=11; (( 1 && (x = 55) )); echo $x'
check arith-ternary-t   '2'         0 'x=1; (( 1 ? (x=2) : (x=3) )); echo $x'
check arith-ternary-f   '3'         0 'x=1; (( 0 ? (x=2) : (x=3) )); echo $x'
check arith-comma       '42
last=6'                             0 'a=(4 5 6); echo $(( a, last = a[2], 42 )); echo last=$last'
# [[ ]] evaluates -eq operands as arithmetic; [ ] does not.
check arith-cond-eq     'yes'       0 'e=1+2; [[ e -eq 3 ]] && echo yes'
check arith-test-noeval 'st=2'      0 'e=1+2; [ e -eq 3 ] 2>/dev/null; echo st=$?'

# --- declaration utilities and command-scoped assignments ------------------
# Expectations diffed against bash before being written down.
# declare/typeset/local are declaration utilities too: no splitting, no glob.
check decl-no-split     '[a b c]' 0 'w="a b c"; declare d=$w; echo "[$d]"'
check decl-no-glob      '[*]'     0 'cd "$TMPDIR" || cd /tmp; touch foo=a foo=b; typeset foo=*; echo "[$foo]"; rm -f foo=a foo=b'
# `cd ""' is an error, not $HOME -- and must not crash reaching for char 0.
check_err cd-null-dir   'null directory' 1 'cd ""'
# `declare -a' on an existing array declares the type, it does not reset it.
check decl-a-keeps      'n=3'     0 'declare -a arr; arr=(a b c); declare -a arr; echo n=${#arr[@]}'
check decl-A-keeps      'n=2'     0 'declare -A d; d[x]=1; d[y]=2; declare -A d; echo n=${#d[@]}'
# `$x' on an array is `${x[0]}', so no element 0 means UNSET.
check decl-a-unset      '[]'      0 'declare -a x; echo "[${x+SET}]"'
# readonly must be enforced before the array is mutated, not after.
check ro-array-append   '[1 2 3]' 0 'a=(1 2 3); readonly -a a; eval "a+=(4)" 2>/dev/null; echo "[${a[@]}]"'
check ro-array-elem     '[1 2 3]' 0 'a=(1 2 3); readonly -a a; eval "a[0]=9" 2>/dev/null; echo "[${a[@]}]"'
# eval contains the abort; a bare assignment is still fatal (see posix-diff).
check ro-eval-continues '[1]'     0 'readonly r=1; eval "r=2" 2>/dev/null; echo "[$r]"'
# A prefix assignment is in the command's ENVIRONMENT, so children see it --
# including children of a shell function, and of a nested call.
check temp-exported     'v'       0 'f(){ printenv X; }; X=v f'
check temp-nested       'f
g1'                               0 'g(){ printenv F G1; }; f(){ G1=g1 g; }; F=f f'
# unset on a temporarily-bound name reveals what it shadowed.
check temp-unset-reveal 'x=temp
after=global
end=global'                       0 'f(){ echo "x=$x"; unset x; echo "after=$x"; }; x=global; x=temp f; echo "end=$x"'
check temp-not-leaked   'st=1'    0 'X=1 true; printenv X; echo st=$?'

# A token that can only CLOSE a construct is a syntax error at top level, not
# a command name -- and must not silently discard the rest of the line.
check_err syn-do        ''  2 'do'
check_err syn-done      ''  2 'done'
check_err syn-fi        ''  2 'fi'
check_err syn-esac      ''  2 'esac'
check_err syn-rbrace    ''  2 '}'
check_err syn-do-midline '' 2 'echo hi; do; echo x'
check_err syn-assign-for '' 2 'FOO=bar for i in a b; do echo $FOO; done'
# ...but an assignment prefix makes an OPENING word an ordinary command name.
check_err syn-assign-bare 'not found' 127 'FOO=bar for'
# Constructs that legitimately consume a closer are untouched.
check syn-brace-group   'hi'      0 '{ echo hi; }'
check syn-case          'm'       0 'case x in x) echo m;; esac'
check syn-subst-group   'a'       0 'x=$( { echo a; } ); echo "$x"'

# --- read: option clusters, -n counting, IFS field counts -----------------
# Every expectation here was diffed against bash before being written down.
check read-smooshed     'v=h'    0 'echo hi | { read -rn1 v; echo "v=$v"; }'
check read-rd-empty     '[foo
bar]'                            0 "printf 'foo\nbar\n' | { read -rd '' v; echo \"[\$v]\"; }"
# -n counts characters AFTER escape processing, so \b\c\d collapse first.
check read-n-postescape '[abcde]' 0 "echo 'a\b\c\d\e\f' | { read -n 5; echo \"[\$REPLY]\"; }"
check read-n-incomplete '[abcd]'  0 "echo 'abc\def\ghijklmn' | { read -n 4; echo \"[\$REPLY]\"; }"
check read-n-zero       '[]'      0 "echo 'a\b' | { read -n 0; echo \"[\$REPLY]\"; }"
# backslash-newline is a continuation whatever -d says
check read-d-continues  '[a bc d
]'                               0 "printf '%s\n' 'a b\\' 'c d' | { read -d , ; echo \"[\$REPLY]\"; }"
# A -p prompt goes to a terminal only; on a pipe it would corrupt stderr.
# read's stderr is folded into stdout here so the prompt WOULD show if emitted
# -- an empty-`want' check_err would pass either way and prove nothing.
check read-p-no-tty     'v=hi'    0 'echo hi | { read -p PROMPT v 2>&1; echo "v=$v"; }'
# Usage errors: bash separates a bad option (2) from a bad number (1).
check_err read-bad-opt  'invalid option' 2 'read -z </dev/null'
check_err read-bad-num  'invalid number' 1 'read -n abc </dev/null'
check read-bad-num-st   'st=1'   0 'echo hi | { read -n abc; }; echo "st=$?"' 
check read-bad-timeout  'st=1'   0 'echo hi | { read -t -0.5; }; echo "st=$?"'

# An at-expansion yielding nothing must produce NO field, for arrays as well
# as for "$@" -- `read -a' on an empty line otherwise gave one empty field.
check at-empty-array    'count=0' 0 'a=(); set -- "${a[@]}"; echo "count=$#"'
check at-unset-array    'count=0' 0 'set -- "${nope[@]}"; echo "count=$#"'
check at-read-a-empty   'count=0' 0 'echo "" | { read -a arr; set -- "${arr[@]}"; echo "count=$#"; }'
check at-plain-empty    'count=1' 0 'set -- ""; echo "count=$#"'

# A trailing IFS non-whitespace delimiter generates no field (POSIX 2.6.5), so
# `xx' is exactly two fields and b gets "" rather than the raw remainder.
check read-ifs-exact2   '[][]'    0 'IFS="x "; echo xx | { read a b; echo "[$a][$b]"; }'
check read-ifs-overflow '[][xx]'  0 'IFS="x "; echo xxx | { read a b; echo "[$a][$b]"; }'
check read-ifs-trailing '[][a]'   0 'IFS="x "; echo "xax   " | { read a b; echo "[$a][$b]"; }'
check read-ifs-remain   '[][axx]' 0 'IFS="x "; echo "xaxx  " | { read a b; echo "[$a][$b]"; }'
# An escaped space is quoted: it forms a field and survives the trailing trim.
check read-esc-space    '[][ ]'   0 'IFS="x "; echo "x \\ " | { read a b; echo "[$a][$b]"; }'
check read-esc-space2   '[][ ]'   0 'IFS="x "; echo "x\\  " | { read a b; echo "[$a][$b]"; }'

# --- echo -e escape decoding ----------------------------------------------
# echo and printf share DECODE-ESCAPE-AT; these pin the forms that differ or
# that a partial decoder silently passes through as literal text.
check echo-e-hex        'abcdef'  0 'echo -e "abcd\x65f"'
check echo-e-hex-1digit 'ab'      0 'echo -e "a\x62"'
check echo-e-hex-bare   'a\x'     0 'echo -e "a\x"'
check echo-e-esc        'a b'     0 'echo -e "a\eb" | tr "\033" " "'
check echo-e-u4         'abcdef'  0 'echo -e "abcdef"'
check echo-e-u8         'abcdef'  0 'echo -e "abcd\U00000065f"'
check echo-e-u-short    'ab'      0 'echo -e "a\u62"'
# \c stops output AND suppresses the newline, so the next echo joins on.
check echo-e-c-stops    'xy abZ'  0 'echo -e "xy ab\cde zzz"; echo Z'
# Octal needs the leading zero in echo: \44 is literal, \044 is a byte.
check echo-e-octal      'abcd$e'  0 'echo -e "abcd\0044e"'
check echo-e-octal-1    'a b'     0 'echo -e "a\040b"'
check echo-e-no-octal   'a\44'    0 'echo -e "a\44"'
# printf keeps the bare-\NNN form its format string is specified with.
check printf-bare-octal 'a b'     0 'printf "a\040b\n"'
check printf-hex        'abc'     0 'printf "a\x62c\n"'

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

# --- runaway recursion fails as a shell, not as a Lisp backtrace ----------
# Found by fuzzing: `f()${}f' followed by a call to f exhausted the control
# stack and dumped SBCL's own error.
check_err recursion-limit 'maximum function nesting level exceeded' 1 'f() { f; }; f'
check recursion-bounded '3
2
1'                              0 'f() { [ $1 -le 0 ] && return; echo $1; f $(($1-1)); }; f 3'

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
printf '#!/usr/bin/env sxsh\necho script-mode\necho "$1"\n' > "$tmp/s.sh"
got=$("$SXSH" "$tmp/s.sh" firstarg 2>&1); got_st=$?
if [ "$got" = "script-mode
firstarg" ] && [ "$got_st" -eq 0 ]; then
  pass=$((pass + 1))
else
  fail=$((fail + 1))
  printf 'FAIL script-mode\n  want: %q\n  got:  %q (status %d)\n' \
    "script-mode
firstarg" "$got" "$got_st"
fi

# --- control flow must not escape its execution environment ---------------
# Every case here once escaped a boundary it should not cross. The shape that
# hides such a bug is a construct in a NON-FINAL position -- `return' looked
# correct for months because every test used it as the last command -- so each
# case is followed by something that must, or must not, still run.

# A subshell is a separate execution environment: return/break/continue end
# the subshell and become its status. bash, zsh, dash and bash 3.2 agree.
check cf-sub-return       'st=4'            0 'f() { (return 4); echo st=$?; }; f'
check cf-sub-return-nf    'st=4'            0 'f() { (return 4; echo NO); echo st=$?; }; f'
check cf-sub-return-after 'AFTER'           0 'f() { (return 4); echo AFTER; }; f'
check cf-sub-exit         'st=5'            0 'f() { (exit 5; echo NO); echo st=$?; }; f'
check cf-sub-break        'IN
IN
done'                                       0 'for i in 1 2; do (break); echo IN; done; echo done'
check cf-sub-break-nf     'st=0
st=0'                                       0 'for i in 1 2; do (break; echo NO); echo st=$?; done'
check cf-sub-continue     'IN
IN
done'                                       0 'for i in 1 2; do (continue); echo IN; done; echo done'

# So is a command substitution.
check cf-csub-return      '[] AFTER'        0 'f() { x=$(return 4; echo NO); echo "[$x] AFTER"; }; f'
check cf-csub-exit        '[] after'        0 'x=$(exit 3; echo NO); echo "[$x] after"'
check cf-csub-break       'IN[]
IN[]
done'                                       0 'for i in 1 2; do x=$(break; echo NO); echo "IN[$x]"; done; echo done'

# So is each stage of a pipeline (POSIX XCU 2.9.2). `echo a | { exit 4; }'
# sets the pipeline's status; it does not exit the shell.
check cf-pipe-return      'AFTER
st=0'                                       0 'f() { echo a | { return 3; }; echo AFTER; }; f; echo st=$?'
check cf-pipe-exit        'AFTER st=4'      0 'echo a | { exit 4; }; echo AFTER st=$?'
check cf-pipe-break       'IN
IN
done'                                       0 'for i in 1 2; do echo a | { break; }; echo IN; done; echo done'

# `return' must reach the function, not stop at the builtin. Catching it in
# RUN-BUILTIN made `return' merely the builtin's exit status, so control
# carried straight on to the next command.
check cf-return-nonfinal  ''                1 'f() { return 1; echo NO; }; f'
check cf-return-nested    'AFTER-G'         0 'f() { g() { return 1; echo NO; }; g; echo AFTER-G; }; f'
check cf-return-in-loop   'st=2'            0 'f() { while true; do return 2; done; echo NO; }; f; echo st=$?'
check cf-return-in-case   'st=3'            0 'f() { case a in a) return 3; echo NO;; esac; echo NO2; }; f; echo st=$?'
check cf-return-in-brace  'st=3'            0 'f() { { return 3; }; echo NO; }; f; echo st=$?'
check cf-return-andor     'st=3'            0 'f() { true && return 3; echo NO; }; f; echo st=$?'
check cf-break-n          'done'            0 'for i in 1 2 3; do for j in a b; do break 2; done; echo NO; done; echo done'
check cf-continue-n       'done'            0 'for i in 1 2; do for j in a b; do continue 2; done; echo NO; done; echo done'

# `exit' ends the shell from anywhere, including a non-final position.
check cf-exit-in-func     ''                3 'f() { exit 3; echo NO; }; f; echo NO2'
check cf-exit-in-loop     ''                3 'for i in 1 2; do exit 3; echo NO; done; echo NO2'
check cf-exit-in-brace    ''                3 '{ exit 3; echo NO; }; echo NO2'
check cf-exit-in-case     ''                3 'case x in x) exit 3; echo NO;; esac; echo NO2'

# `eval' and `.' run inline in the current environment, so control flow must
# pass straight through them. Routing both through RUN -- which backstops
# SHELL-EXIT and FUNC-RETURN -- made `eval "exit 0"' a silent no-op, which is
# how libtool's --config ran past its own `exit' into a usage error.
check cf-eval-exit        ''                6 'eval "exit 6"; echo NO'
check cf-eval-return      'st=5'            0 'f() { eval "return 5"; echo NO; }; f; echo st=$?'
check cf-eval-break       'done'            0 'for i in 1 2; do eval break; echo NO; done; echo done'
check cf-eval-continue    'done'            0 'for i in 1 2 3; do eval continue; echo NO; done; echo done'
check cf-eval-exit-nested ''                0 'f() { exit 0; }; g() { eval f; echo NO; }; g; echo NO2'
check cf-eval-exit-arg    ''                0 'h() { eval "$1"; }; f() { exit 0; }; h f; echo NO'
check cf-eval-status      'a
b
st=0'                                       0 'eval "echo a; echo b"; echo st=$?'
check cf-eval-false       'st=1'            0 'eval "false"; echo st=$?'
check cf-eval-empty       'st=0'            0 'eval ""; echo st=$?'

# `return' in a sourced script ends the script and becomes its status; the
# caller resumes. `exit' and `break' keep going (bash and dash agree).
check cf-dot-return       'in-dot
st=5'                                       0 'printf "echo in-dot\nreturn 5\necho NO\n" >"$SMOKE_TMP/d.sh"; . "$SMOKE_TMP/d.sh"; echo st=$?'
check cf-dot-return-func  'in-dot
AFTER st=5'                                 0 'printf "echo in-dot\nreturn 5\necho NO\n" >"$SMOKE_TMP/d.sh"; f() { . "$SMOKE_TMP/d.sh"; echo "AFTER st=$?"; }; f'
check cf-dot-exit         'in-dot'          6 'printf "echo in-dot\nexit 6\necho NO\n" >"$SMOKE_TMP/d.sh"; . "$SMOKE_TMP/d.sh"; echo NO2'
check cf-dot-break        'in-dot
done'                                       0 'printf "echo in-dot\nbreak\necho NO\n" >"$SMOKE_TMP/d.sh"; for i in 1 2; do . "$SMOKE_TMP/d.sh"; echo NO2; done; echo done'

# --- ${var#pat}: the pattern operand is expanded ---------------------------
# EXPAND-NESTED-PATTERN used to return the operand untouched, so `${s#$p}'
# hunted for the literal two characters `$p'. Any script that walks a string a
# character at a time then loops forever -- which is how gpgrt-config hung.
check pat-var-prefix    'ello'          0 's=hello; p=h; echo "${s#$p}"'
check pat-var-suffix    'hell'          0 's=hello; p=o; echo "${s%$p}"'
check pat-var-longest   'b.c|c'         0 's=a.b.c; p="*."; echo "${s#$p}|${s##$p}"'
check pat-var-suf-long  'a.b|a'         0 's=a.b.c; p=".*"; echo "${s%$p}|${s%%$p}"'
check pat-cmdsub        'ello'          0 's=hello; echo "${s#$(printf %c "$s")}"'
check pat-backquote     'ello'          0 's=hello; echo "${s#`echo h`}"'
check pat-nested-param  'ello'          0 's=hello; echo "${s#${p:-h}}"'
check pat-positional    'ello'          0 'set -- h; s=hello; echo "${s#$1}"'
check pat-unset-var     'hello'         0 's=hello; echo "${s#$nosuchvar_zz}"'
# metacharacters from an expansion stay live; ones quoted in the source do not
check pat-quoted-star   'x'             0 's="*x"; echo "${s#"*"}"'
check pat-escaped-star  ''              0 's=a*b; p="a\*b"; echo "${s#$p}"'
check pat-literal-still 'llo|a.b|c'     0 's=hello; t=a.b.c; echo "${s#he}|${t%.*}|${t##*.}"'
# the loop gpgrt-config's substitute_vars runs: it must terminate
check pat-char-walk     'hello'         0 's=hello; r=""; while [ -n "$s" ]; do c=$(printf %c "$s"); r="$r$c"; s="${s#$c}"; done; echo "$r"'

# --- read leaves the file offset just past the newline ---------------------
# `read' used to go through the buffered *STANDARD-INPUT*, which slurped the
# whole file on the first call. A redirected `read' inside a `while read' loop
# was then served stale bytes from the outer input.
check read-inner-redir  'a=V
b=V
c=V'                                    0 'printf "V\n" >"$SMOKE_TMP/f1"; printf "a\nb\nc\n" >"$SMOKE_TMP/f2"; while read l; do read X <"$SMOKE_TMP/f1"; echo "$l=$X"; done <"$SMOKE_TMP/f2"'
check read-inner-heredoc 'a/Va
b/Vb
c/Vc'                                   0 'printf "a\nb\nc\n" | while read l; do read X <<EOF
V$l
EOF
echo "$l/$X"; done'
check read-then-cat     'rest1
rest2'                                  0 'printf "a\nrest1\nrest2\n" >"$SMOKE_TMP/f4"; { read first; cat; } <"$SMOKE_TMP/f4"'
check read-fd-dup       'l1/l2'         0 'printf "l1\nl2\n" >"$SMOKE_TMP/f3"; exec 3<"$SMOKE_TMP/f3"; read x <&3; read y <&3; echo "$x/$y"; exec 3<&-'

# --- local -----------------------------------------------------------------
# Not in POSIX Issue 7, but bash, dash, ksh and zsh all have it and real
# scripts require it: git's t/test-lib.sh alone uses it 41 times, and without
# it those scripts abort on `local: command not found' rather than misbehave.
# Semantics follow bash and dash, which agree wherever it matters.
check local-basic       'in
out'                                    0 'f() { local x=in; echo $x; }; x=out; f; echo $x'
check local-no-value    '[UNSET]
out'                                    0 'f() { local x; echo "[${x-UNSET}]"; }; x=out; f; echo $x'
check local-multiple    '12'            0 'f() { local x=1 y=2; echo "$x$y"; }; f'
check local-dynamic     'see=in'        0 'g() { echo "see=$x"; }; f() { local x=in; g; }; x=out; f'
check local-after-ret   'st=3 x=out'    0 'f() { local x=in; return 3; }; x=out; f; echo "st=$? x=$x"'
check local-callee-sets 'out'           0 'f() { local x=in; g; }; g() { x=changed; }; x=out; f; echo $x'
check local-nested      'after=a'       0 'f() { local x=a; h; echo "after=$x"; }; h() { local x=b; }; f'
check local-redeclare   '2
out'                                    0 'f() { local x=1; local x=2; echo $x; }; x=out; f; echo $x'
check local-unset       '[U]
[out]'                                  0 'f() { local x=1; unset x; echo "[${x-U}]"; }; x=out; f; echo "[$x]"'
check local-quoted-val  '[with space]'  0 'f() { local "x=with space"; echo "[$x]"; }; f'
check local-from-arg    'hello'         0 'f() { local x=$1; echo $x; }; f hello'
# restored however the function ends, not just on a normal return
check local-errexit     '[out]'         0 'f() { local x=in; false; }; set -e; x=out; f || true; echo "[$x]"'
check_err local-outside 'can only be used in a function' 1 'local z=1'

# --- assignment prefixes are command-scoped, except on special builtins ----
# POSIX 2.9.1. sxsh persisted them for every command type, so
# `GIT_DIR=x git init' left GIT_DIR set -- which is exactly how one failing
# git test poisoned three later ones. The reference here is `bash --posix':
# plain bash does not persist even for special builtins, which is one of the
# deviations `set -o posix' exists to fix.
check assign-external   '[U]'           0 'V=x /bin/echo -n; echo "[${V-U}]"'
check assign-regular    '[U]'           0 'V=x true; echo "[${V-U}]"'
check assign-builtin-cd '[U]'           0 'V=x cd .; echo "[${V-U}]"'
check assign-function   '[U]'           0 'f() { :; }; V=x f; echo "[${V-U}]"'
check assign-overwrite  '[old]'         0 'V=old; V=new /bin/echo hi >/dev/null; echo "[$V]"'
check assign-cmdsub     '[U][U]'        0 'W=$(pwd) D=zz /bin/echo hi >/dev/null; echo "[${W-U}][${D-U}]"'
# special builtins DO persist
check assign-special    '[x]'           0 'V=x :; echo "[${V-U}]"'
check assign-special-ex '[x]'           0 'V=x export Z=1; echo "[${V-U}]"'
# ...but the binding must still be visible while the command runs
check assign-visible    'a|b'           0 'printf "a:b\n" | { IFS=: read x y; echo "$x|$y"; }'
check assign-seen-func  'in=x
after=[U]'                              0 'f() { echo "in=$V"; }; V=x f; echo "after=[${V-U}]"'
check assign-left-right '1'             0 'a=1 b=$a /bin/sh -c "echo \$b"'
check assign-in-env     'V=x'           0 'V=x env | grep "^V=" | head -1'

# --- bash extensions (tranche 1) -------------------------------------------
# Not POSIX. Verified against bash case by case; see CLAUDE.md for the
# inventory of what is done and what is still missing.

# ${x/pat/rep} and friends
check bx-subst-first    'baa'           0 'v=aaa; echo ${v/a/b}'
check bx-subst-all      'bbb'           0 'v=aaa; echo ${v//a/b}'
check bx-subst-head     'Xaa'           0 'v=aaa; echo ${v/#a/X}'
check bx-subst-tail     'aaX'           0 'v=aaa; echo ${v/%a/X}'
check bx-subst-delete   'ac'            0 'v=abc; echo ${v/b}'
check bx-subst-dots     'a-b-c'         0 'v=a.b.c; echo ${v//./-}'
check bx-subst-longest  'X'             0 'v=aaa; echo ${v/a*/X}'
check bx-subst-class    'aXc'           0 'v=abc; echo ${v/[bc]/X}'
check bx-subst-var-pat  'aXc'           0 'v=abc; p=b; echo ${v/$p/X}'
check bx-subst-slash    'x-y'           0 'v=x/y; echo ${v/\//-}'
# ${x^^} case mapping
check bx-upper-all      'AB'            0 'v=ab; echo "${v^^}"'
check bx-lower-all      'ab'            0 'v=AB; echo "${v,,}"'
check bx-upper-first    'Ab'            0 'v=ab; echo "${v^}"'
check bx-lower-first    'aB'            0 'v=AB; echo "${v,}"'
check bx-upper-pattern  'hEllO'         0 'v=hello; echo "${v^^[eo]}"'
check bx-case-empty     '[]'            0 'v=; echo "[${v^^}]"'
# ${!x} indirection and ${!prefix*}
check bx-indirect       'z'             0 'x=y; y=z; echo ${!x}'
check bx-indirect-unset '[]'            0 'x=nosuch_zz; echo "[${!x}]"'
check bx-prefix-names   'pfxa pfxb'     0 'pfxa=1; pfxb=2; echo ${!pfx*}'
# name+=value
check bx-append         'ab'            0 'v=a; v+=b; echo $v'
check bx-append-unset   'x'             0 'v+=x; echo $v'
check bx-append-quoted  'a b'           0 'v=a; v+=" b"; echo "$v"'
check bx-append-prefix  'abc'           0 'v=a; v+=b v2=c; echo "$v$v2"'
check bx-append-notword 'a+=b'          0 'echo a+=b'
# set -o pipefail
check bx-pipefail-first '1'             0 'set -o pipefail; false | true; echo $?'
check bx-pipefail-last  '1'             0 'set -o pipefail; true | false; echo $?'
check bx-pipefail-off   '0'             0 'false | true; echo $?'
check bx-pipefail-rightmost '4'         0 'set -o pipefail; sh -c "exit 3" | sh -c "exit 4"; echo $?'
check bx-pipefail-clear '0'             0 'set -o pipefail; set +o pipefail; false | true; echo $?'
check bx-pipefail-ok    '0'             0 'set -o pipefail; true | true; echo $?'
# $RANDOM / $SECONDS. Assigning to RANDOM seeds it rather than replacing it,
# and unsetting either one destroys the dynamic behaviour permanently.
check bx-random-range   'y'             0 'test "$RANDOM" -ge 0 && test "$RANDOM" -lt 32768 && echo y'
check bx-random-seeded  'y'             0 'RANDOM=5; test "$RANDOM" -ge 0 && echo y'
check bx-random-unset   '[]'            0 'unset RANDOM; echo "[$RANDOM]"'
# after unset it is an ordinary variable, so the assignment sticks as a value
check bx-random-reseed  '[5]'           0 'unset RANDOM; RANDOM=5; echo "[$RANDOM]"'
check bx-seconds        'y'             0 'test "$SECONDS" -ge 0 && echo y'
check bx-seconds-assign '99'            0 'SECONDS=99; echo $SECONDS'
check bx-seconds-unset  '[]'            0 'unset SECONDS; echo "[$SECONDS]"'

# --- bash extensions (tranche 2: parser-level) -----------------------------
# here-strings
check bx-herestring     'hello'         0 'cat <<<hello'
check bx-herestring-sp  'a b'           0 'cat <<<"a b"'
check bx-herestring-var 'x'             0 'v=x; cat <<<"$v"'
check bx-herestring-read 'hi'           0 'read x <<<hi; echo $x'
# &> and &>> send stdout AND stderr to one file
check bx-amp-redirect   'ok'            0 'echo hi &>/dev/null; echo ok'
check bx-amp-both       '2'             0 'sh -c "echo o; echo e >&2" &>"$SMOKE_TMP/ae"; wc -l <"$SMOKE_TMP/ae" | tr -d " "'
check bx-amp-append     '4'             0 'sh -c "echo o; echo e >&2" &>"$SMOKE_TMP/ap"; sh -c "echo o; echo e >&2" &>>"$SMOKE_TMP/ap"; wc -l <"$SMOKE_TMP/ap" | tr -d " "'
# fd 2 must be restored afterwards, or later stderr would vanish too
check_err bx-amp-restores 'after'       0 'echo hi &>/dev/null; echo after >&2'
# |& is `2>&1 |'
check bx-pipe-amp       'hi'            0 'echo hi |& cat'
check bx-pipe-amp-both  '2'             0 'sh -c "echo o; echo e >&2" |& wc -l | tr -d " "'
check bx-pipe-amp-brace 'x'             0 '{ echo x; } |& cat'
# function keyword, with and without ()
check bx-function-kw    'hi'            0 'function f { echo hi; }; f'
check bx-function-kw-p  'hi'            0 'function f() { echo hi; }; f'
check bx-function-args  'arg'           0 'function g { echo $1; }; g arg'
# brace expansion -- happens before every other expansion
check bx-brace-list     'a b c'         0 'echo {a,b,c}'
check bx-brace-range    '1 2 3 4 5'     0 'echo {1..5}'
check bx-brace-rev      '5 4 3 2 1'     0 'echo {5..1}'
check bx-brace-step     '0 5 10'        0 'echo {0..10..5}'
check bx-brace-alpha    'a b c d e'     0 'echo {a..e}'
check bx-brace-affix    'xay xby'       0 'echo x{a,b}y'
check bx-brace-nested   'a b c'         0 'echo {a,{b,c}}'
check bx-brace-cross    'a1 a2 b1 b2'   0 'echo {a,b}{1,2}'
check bx-brace-empty-alt 'a'            0 'echo {a,}'
# not brace groups: no comma, no range, or quoted
check bx-brace-single   '{a}'           0 'echo {a}'
check bx-brace-none     '{}'            0 'echo {}'
check bx-brace-dquote   '{a,b}'         0 'echo "{a,b}"'
check bx-brace-squote   '{a,b}'         0 "echo '{a,b}'"
check bx-brace-escaped  '{a,b}'         0 'echo \{a,b\}'
check bx-brace-lone     '{'             0 'echo {'

# --- bash extensions (tranche 3: builtins) ---------------------------------
# printf -v NAME puts the result in a variable instead of on stdout
check bx-printf-v       'hi'            0 'printf -v v %s hi; echo $v'
check bx-printf-v-multi 'a-b'           0 'printf -v v "%s-%s" a b; echo $v'
check bx-printf-v-joined 'hi'           0 'printf -vv %s hi; echo $v'
check bx-printf-v-silent 'x'            0 'printf -v v %s noise; echo x'
# printf %q renders a value so it re-parses to itself. The escape set was
# taken by probing bash, not guessed: it excludes # ~ = % and :.
check bx-printf-q-space 'a\ b'          0 'printf "%q\n" "a b"'
check bx-printf-q-quote "it\\'s"        0 'printf "%q\n" "it'"'"'s"'
check bx-printf-q-plain 'x'             0 'printf "%q\n" x'
check bx-printf-q-empty "''"            0 'printf "%q\n" ""'
check bx-printf-q-dollar 'a\$b'         0 'printf "%q\n" "a\$b"'
check bx-printf-q-hash  'a#b'           0 'printf "%q\n" "a#b"'
check bx-printf-q-tab   "\$'a\\tb'"     0 'printf "%q\n" "$(printf "a\tb")"'
check bx-printf-q-round 'a b'           0 'v=$(printf "%q" "a b"); eval "echo $v"'
# read -n (at most N chars) / -N (exactly N, no delimiter, no IFS trimming)
check bx-read-n         '[123]'         0 'printf 12345 | { read -n 3 x; echo "[$x]"; }'
check bx-read-n-stops   '[ab]'          0 'printf "ab\ncd\n" | { read -n 10 x; echo "[$x]"; }'
check bx-read-n-joined  '[123]'         0 'printf 12345 | { read -n3 x; echo "[$x]"; }'
check bx-read-n-fields  '[a][b]'        0 'printf "a b\n" | { read -n 3 x y; echo "[$x][$y]"; }'
check bx-read-N-nodelim '[a
b]'                                     0 'printf "a\nb\nc\n" | { read -N 3 x; echo "[$x]"; }'
check bx-read-N-short   'st=1 [ab]'     0 'printf "ab" | { read -N 5 x; echo "st=$? [$x]"; }'
check bx-read-N-noifs   '[  ab  ]'      0 'printf "  ab  \n" | { read -N 6 x; echo "[$x]"; }'
# read -d alternate delimiter
check bx-read-d         '[a]'           0 'printf a:b | { read -d : x; echo "[$x]"; }'
check bx-read-d-twice   '[a][b]'        0 'printf "a:b:c" | { read -d : x; read -d : y; echo "[$x][$y]"; }'
check bx-read-d-eof     'st=1 [foo]'    0 'printf "foo" | { read -d : x; echo "st=$? [$x]"; }'
# read -u FD, -t timeout, -s silent
check bx-read-u         '[hi]'          0 'read -u 3 3<<<hi; echo "[$REPLY]"'
check bx-read-u-d       '[a]'           0 'read -u 3 -d : x 3<<<"a:b"; echo "[$x]"'
check bx-read-t-poll    '0'             0 'read -t 0 </dev/null; echo $?'
check bx-read-s         '[hi]'          0 'echo hi | { read -s x; echo "[$x]"; }'

# --- bash extensions (tranche 4: syntax) -----------------------------------
# case fall-through: `;&' runs the next body without testing it, `;;&' keeps
# testing the patterns that follow
check bx-case-fall      '1
2'                                      0 'case a in a) echo 1;& b) echo 2;; esac'
check bx-case-retest    '1
2'                                      0 'case a in a) echo 1;;& a*) echo 2;; esac'
check bx-case-fall-chain '1
2
3'                                      0 'case a in a) echo 1;& b) echo 2;& c) echo 3;; esac'
check bx-case-retest-miss '1'           0 'case a in a) echo 1;;& z) echo 2;; esac'
check bx-case-mixed     '1
2
3'                                      0 'case ab in a*) echo 1;;& *b) echo 2;;& *) echo 3;; esac'
check bx-case-fall-empty '2'            0 'case a in a) ;& b) echo 2;; esac'
check bx-case-plain     '1'             0 'case a in a) echo 1;; b) echo 2;; esac'
# ((expr)) -- status is INVERTED from the value, so 0 is false as in C
check bx-arith-cmd      'y'             0 '((1+1)) && echo y'
check bx-arith-false    'n'             0 '((0)) && echo y || echo n'
check bx-arith-status1  '0'             0 '((1)); echo $?'
check bx-arith-status0  '1'             0 '((0)); echo $?'
check bx-arith-compare  'big'           0 'x=5; ((x > 3)) && echo big'
check bx-arith-incr     '1'             0 'x=0; ((x++)); echo $x'
check bx-arith-preincr  '1'             0 'x=0; ((++x)); echo $x'
check bx-arith-mul-eq   '6'             0 'x=2; ((x*=3)); echo $x'
check bx-arith-if       'lt'            0 'if ((1<2)); then echo lt; fi'
check bx-arith-while    '123'           0 'x=1; while ((x<=3)); do printf %s $x; ((x++)); done; echo'
# for ((init; cond; step))
check bx-arith-for      '012'           0 'for ((i=0;i<3;i++)); do printf %s $i; done; echo'
check bx-arith-for-down '321'           0 'for ((i=3;i>0;i--)); do printf %s $i; done; echo'
check bx-arith-for-step '024'           0 'for ((i=0;i<5;i+=2)); do printf %s $i; done; echo'
check bx-arith-for-break '01'           0 'for ((i=0;i<5;i++)); do [ $i -eq 2 ] && break; printf %s $i; done; echo'
check bx-arith-for-cont '023'           0 'for ((i=0;i<4;i++)); do [ $i -eq 1 ] && continue; printf %s $i; done; echo'
check bx-arith-for-nest '00 01 10 11 '  0 'for ((i=0;i<2;i++)); do for ((j=0;j<2;j++)); do printf "%s%s " $i $j; done; done; echo'
check bx-arith-for-noinit '910'         0 'i=9; for ((;i<11;i++)); do printf %s $i; done; echo'
check bx-arith-for-nosemi '012'         0 'for ((i=0;i<3;i++)) do printf %s $i; done; echo'
# (( )) must not swallow nested subshells
check bx-subshell-nest  'a
b'                                      0 '( (echo a); (echo b) )'
check bx-subshell-deep  'n'             0 '( ( echo n ) )'

# --- bash extensions (tranche 5: arrays) -----------------------------------
# Arrays are sparse, so both flavours are hash tables: `a[5]=x' on an empty
# array leaves 0-4 genuinely absent rather than empty.
check bx-array-index    '2'             0 'a=(1 2 3); echo ${a[1]}'
check bx-array-all      '1 2 3'         0 'a=(1 2 3); echo "${a[@]}"'
check bx-array-star     '1 2 3'         0 'a=(1 2 3); echo "${a[*]}"'
check bx-array-count    '3'             0 'a=(1 2 3); echo ${#a[@]}'
check bx-array-keys     '0 1 2'         0 'a=(1 2 3); echo ${!a[@]}'
check bx-array-scalar   '1'             0 'a=(1 2 3); echo $a'
check bx-array-negative '3'             0 'a=(1 2 3); echo ${a[-1]}'
check bx-array-elem-len '3'             0 'a=(x abc); echo ${#a[1]}'
# quoting inside a literal must survive: the RHS reaches the executor RAW,
# because pre-expanding it would strip the quotes and split "b c" in two
check bx-array-quoted   '[1]
[b c]
[3]'                                    0 'a=(1 "b c" 3); for x in "${a[@]}"; do echo "[$x]"; done'
check bx-array-quoted-n '2'             0 'a=(1 "b c"); echo ${#a[@]}'
check bx-array-split    '2'             0 'x="1 2"; a=($x); echo ${#a[@]}'
check bx-array-nosplit  '1'             0 'x="1 2"; a=("$x"); echo ${#a[@]}'
# append, element assignment, sparseness, unset
check bx-array-append   '1 2 3 4 5'     0 'a=(1 2 3); a+=(4 5); echo "${a[@]}"'
check bx-array-elem-set '0 5'           0 'a=(1); a[5]=z; echo "${!a[@]}"'
check bx-array-sparse-n '2'             0 'a=(1); a[5]=z; echo ${#a[@]}'
check bx-array-unset-el '0 2'           0 'a=(1 2 3); unset "a[1]"; echo "${!a[@]}"'
check bx-array-unset-all '[]'           0 'a=(1 2 3); unset a; echo "[${a[@]}]"'
check bx-array-explicit '0 2 / y x'     0 'a=([2]=x [0]=y); echo "${!a[@]} / ${a[@]}"'
# associative arrays
check bx-assoc          'v'             0 'declare -A m; m[k]=v; echo ${m[k]}'
check bx-assoc-literal  '12'            0 'declare -A m; m=([a]=1 [b]=2); echo "${m[a]}${m[b]}"'
check bx-assoc-keys     'x'             0 'declare -A m; m[x]=1; echo "${!m[@]}"'
check bx-assoc-count    '1'             0 'declare -A m; m[k]=v; echo ${#m[@]}'
# declare/local attributes. -i is an ATTRIBUTE: later assignments too.
check bx-declare-i      '7'             0 'declare -i n; n=3+4; echo $n'
check bx-declare-i-init '2'             0 'declare -i n=1+1; echo $n'
check bx-declare-i-later '5'            0 'declare -i n; n=2; n=n+3; echo $n'
check bx-local-i        '4'             0 'f() { local -i n=2+2; echo $n; }; f'
check bx-local-array    '2'             0 'f() { local -a arr; arr=(1 2); echo ${#arr[@]}; }; f'
check bx-local-array-scope '1 2
9'                                      0 'f() { local -a arr=(1 2); echo "${arr[@]}"; }; arr=(9); f; echo "${arr[@]}"'
# read -a, mapfile, PIPESTATUS
check bx-read-a         '2'             0 'echo "1 2 3" | { read -a arr; echo ${arr[1]}; }'
check bx-read-a-count   '3'             0 'echo "1 2 3" | { read -a arr; echo ${#arr[@]}; }'
check bx-mapfile        '2'             0 'printf "a\nb\n" | { mapfile arr; echo ${#arr[@]}; }'
check bx-readarray      'a'             0 'printf "a\nb\n" | { readarray arr; echo -n "${arr[0]}"; }'
check bx-pipestatus     '1'             0 'false | true; echo ${PIPESTATUS[0]}'
check bx-pipestatus-all '3 4'           0 'sh -c "exit 3" | sh -c "exit 4"; echo "${PIPESTATUS[@]}"'
check bx-pipestatus-one '0'             0 'true; echo ${PIPESTATUS[0]}'
# an array must survive the re-exec used for `{ ... } &'
check bx-array-async    '1 2 3'         0 'a=(1 2 3); { echo "${a[@]}"; } & wait'

# --- bash extensions (tranche 6: [[ ]]) ------------------------------------
# [[ ]] is not `test' with extra spelling: no field splitting, no globbing of
# operands, `<' and `>' compare rather than redirect, the right side of == is
# a PATTERN, and =~ is a regex with capture groups in $BASH_REMATCH.
check bx-dbr-eq         'y'             0 '[[ 1 == 1 ]] && echo y'
check bx-dbr-ne         'n'             0 '[[ a == b ]] || echo n'
check bx-dbr-neq        'y'             0 '[[ a != b ]] && echo y'
check bx-dbr-status     '0'             0 '[[ 1 == 1 ]]; echo $?'
check bx-dbr-status-f   '1'             0 '[[ 1 == 2 ]]; echo $?'
# the right operand of == is a glob pattern unless quoted
check bx-dbr-pattern    'y'             0 '[[ abc == a* ]] && echo y'
check bx-dbr-quoted-pat 'n'             0 '[[ abc == "a*" ]] || echo n'
check bx-dbr-pat-q      'y'             0 'v=abc; [[ $v == a?c ]] && echo y'
check bx-dbr-pat-var    'y'             0 'p="a*"; [[ abc == $p ]] && echo y'
check bx-dbr-single-eq  'y'             0 '[[ abc = a* ]] && echo y'
# =~ regex, and BASH_REMATCH
check bx-dbr-re         'y'             0 '[[ abc =~ ^a ]] && echo y'
check bx-dbr-re-no      'n'             0 '[[ abc =~ ^b ]] || echo n'
check bx-dbr-re-class   'y'             0 '[[ x123y =~ [0-9]+ ]] && echo y'
check bx-dbr-re-groups  'aa-bbb'        0 '[[ aabbb =~ ^(a+)(b+)$ ]] && echo "${BASH_REMATCH[1]}-${BASH_REMATCH[2]}"'
check bx-dbr-re-whole   'b/b'           0 '[[ abc =~ (b) ]] && echo "${BASH_REMATCH[0]}/${BASH_REMATCH[1]}"'
check bx-dbr-re-dot     'y'             0 '[[ axc =~ a.c ]] && echo y'
# a quoted regex operand is literal: the dot must not be a wildcard
check bx-dbr-re-quoted  'y'             0 '[[ "a.c" =~ "a.c" ]] && echo y'
check bx-dbr-re-var     'y'             0 're="^x"; [[ xyz =~ $re ]] && echo y'
check bx-dbr-re-email   'y'             0 '[[ me@h.com =~ ^[a-z]+@[a-z]+\.[a-z]+$ ]] && echo y'
# unary tests, including -v for "is this name set"
check bx-dbr-v          'y'             0 'x=1; [[ -v x ]] && echo y'
check bx-dbr-v-unset    'n'             0 '[[ -v nosuch_zz ]] || echo n'
check bx-dbr-z          'y'             0 '[[ -z "" ]] && echo y'
check bx-dbr-n          'y'             0 '[[ -n a ]] && echo y'
check bx-dbr-file       'y'             0 '[[ -d /tmp ]] && echo y'
check bx-dbr-bang       'y'             0 '[[ ! -z a ]] && echo y'
check bx-dbr-bare-word  'y'             0 '[[ x ]] && echo y'
check bx-dbr-bare-empty 'n'             0 '[[ "" ]] || echo n'
# && || and grouping inside the brackets
check bx-dbr-and        'y'             0 '[[ 1 == 1 && 2 == 2 ]] && echo y'
check bx-dbr-or         'y'             0 '[[ 1 == 2 || 3 == 3 ]] && echo y'
check bx-dbr-group      'y'             0 '[[ ( 1 == 1 ) ]] && echo y'
check bx-dbr-arith      'y'             0 '[[ 2 -gt 1 ]] && echo y'
check bx-dbr-strcmp     'y'             0 '[[ a < b ]] && echo y'
# an unquoted operand is NOT split or globbed, which is the whole point
check bx-dbr-nosplit    'y'             0 'v="a b"; [[ $v == "a b" ]] && echo y'

# --- bash extensions (tranche 7: shopt, ERR/DEBUG, select) -----------------
# shopt is a SEPARATE namespace from `set -o'; bash keeps the two apart.
check bx-shopt-set      'ok'            0 'shopt -s nullglob && echo ok'
check bx-shopt-query    'nullglob            	off' 1 'shopt nullglob'
check bx-shopt-after-set 'nullglob            	on' 0 'shopt -s nullglob; shopt nullglob'
check bx-shopt-status   '1'             0 'shopt -q nullglob; echo $?'
check bx-shopt-status-on '0'            0 'shopt -s nullglob; shopt -q nullglob; echo $?'
check_err bx-shopt-bogus 'invalid shell option name' 1 'shopt bogus_zz'
# nullglob and dotglob must actually take effect, not just be remembered
check bx-nullglob-works '0'             0 'cd "$SMOKE_TMP" && shopt -s nullglob && echo nomatch_zz*x | wc -w | tr -d " "'
check bx-noglob-default '1'             0 'cd "$SMOKE_TMP" && echo nomatch_zz*x | wc -w | tr -d " "'
check bx-dotglob-off    'plain'         0 'cd "$SMOKE_TMP" && mkdir -p dg && cd dg && : >.hidden && : >plain && echo *'
check bx-dotglob-on     '.hidden plain' 0 'cd "$SMOKE_TMP" && mkdir -p dg2 && cd dg2 && : >.hidden && : >plain && shopt -s dotglob && echo *'
# trap ERR / DEBUG. ERR has the same exemptions as `set -e'.
check bx-trap-err       'E
done'                                   0 'trap "echo E" ERR; false; echo done'
check bx-trap-err-ok    'done'          0 'trap "echo E" ERR; true; echo done'
check bx-trap-err-cond  'done'          0 'trap "echo E" ERR; if false; then :; fi; echo done'
check bx-trap-err-oror  'done'          0 'trap "echo E" ERR; false || true; echo done'
check bx-trap-err-bang  'done'          0 'trap "echo E" ERR; ! false; echo done'
check bx-trap-debug     'ok'            0 'trap "" DEBUG; echo ok'
# select. The menu goes to STDERR so the body's stdout stays redirectable.
check bx-select         'got=a'         0 'printf "1\n" | select x in a b; do echo "got=$x"; break; done 2>/dev/null'
check bx-select-2       'got=b'         0 'printf "2\n" | select x in a b; do echo "got=$x"; break; done 2>/dev/null'
check bx-select-bad     'got=[]'        0 'printf "9\n" | select x in a b; do echo "got=[$x]"; break; done 2>/dev/null'
check bx-select-reply   'REPLY=1'       0 'printf "1\n" | select x in a b; do echo "REPLY=$REPLY"; break; done 2>/dev/null'
check bx-select-eof     'ended'         0 'printf "" | select x in a b; do echo no; done 2>/dev/null; echo ended'
check bx-select-args    'got=p'         0 'set -- p q; printf "1\n" | select x; do echo "got=$x"; break; done 2>/dev/null'
# `select' stays usable as a function name -- it is not POSIX-reserved, so a
# conforming script may define one. bash rejects this; we deliberately do not.
check bx-select-as-name 'fn'            0 'select() { echo fn; }; select'

# --- bash extensions (tranche 8: namerefs, extglob) ------------------------
# namerefs: reads and writes through the alias reach the target
check bx-nameref-read   'hi'            0 'f() { local -n r=$1; echo $r; }; v=hi; f v'
check bx-nameref-write  'changed'       0 'f() { local -n r=$1; r=changed; }; v=orig; f v; echo $v'
check bx-nameref-decl   '1'             0 'declare -n r=v; v=1; echo $r'
check bx-nameref-assign '2'             0 'declare -n r=v; r=2; echo $v'
# a cycle must terminate rather than hang (bash warns; we just stop)
check bx-nameref-cycle  '[]'            0 'declare -n a=b; declare -n b=a; echo "[${a}]"'

# extglob. The MATCHER is gated on the shopt; the LEXER is not, because sxsh
# parses a whole script before running it, so `shopt -s extglob' on line 1 has
# not executed when line 2 is lexed. Accepting the shape unconditionally is
# safe: the alternative reading is a syntax error in bash too.
check bx-extglob-opt    'y'             0 'shopt -s extglob; case abc in ab?(c)) echo y;; esac'
check bx-extglob-opt0   'y'             0 'shopt -s extglob; case ab in ab?(c)) echo y;; esac'
check bx-extglob-opt2   'n'             0 'shopt -s extglob; case abcc in ab?(c)) echo y;; *) echo n;; esac'
check bx-extglob-star   'y'             0 'shopt -s extglob; case abccc in ab*(c)) echo y;; esac'
check bx-extglob-plus   'n'             0 'shopt -s extglob; case ab in ab+(c)) echo y;; *) echo n;; esac'
check bx-extglob-at     'y'             0 'shopt -s extglob; case abc in ab@(c|d)) echo y;; esac'
check bx-extglob-at-no  'n'             0 'shopt -s extglob; case abx in ab@(c|d)) echo y;; *) echo n;; esac'
check bx-extglob-not    'y'             0 'shopt -s extglob; case abx in ab!(c)) echo y;; esac'
check bx-extglob-not-h  'y'             0 'shopt -s extglob; case foo.c in !(*.h)) echo y;; esac'
check bx-extglob-not-h2 'n'             0 'shopt -s extglob; case foo.h in !(*.h)) echo y;; *) echo n;; esac'
check bx-extglob-rep    'y'             0 'shopt -s extglob; case abab in +(ab)) echo y;; esac'
check bx-extglob-multi  'y'             0 'shopt -s extglob; case aXbXc in a@(X|Y)b@(X|Y)c) echo y;; esac'
check bx-extglob-dbr    'y'             0 'shopt -s extglob; v=abc; [[ $v == ab?(c) ]] && echo y'
check bx-extglob-subst  'fooZ'          0 'shopt -s extglob; v=fooX; echo ${v/@(X|Y)/Z}'
# with extglob OFF the pattern keeps its ordinary meaning
check bx-extglob-off    'y'             0 'case abx in ab?) echo y;; *) echo n;; esac'

# --- bash extensions (tranche 9: process substitution) ---------------------
# `<(cmd)' runs cmd with one end of a pipe and substitutes /dev/fd/N naming
# the end WE keep. The descriptor stays inheritable so the command being built
# can open it, and is closed once that command finishes -- leaving it open
# leaks a descriptor and keeps the pipe alive so a reader never sees EOF.
check bx-procsub        'hi'            0 'cat <(echo hi)'
check bx-procsub-two    'one
two'                                    0 'cat <(echo one) <(echo two)'
check bx-procsub-diff   'same'          0 'diff <(echo a) <(echo a) >/dev/null && echo same'
check bx-procsub-differ 'differ'        0 'diff <(echo a) <(echo b) >/dev/null || echo differ'
check bx-procsub-pipe   '3'             0 'cat <(printf "a\nb\nc\n") | wc -l | tr -d " "'
check bx-procsub-cmdsub 'x'             0 'v=$(cat <(echo x)); echo $v'
# as a redirection target, not just an argument
check bx-procsub-redir  '2'             0 'wc -l < <(printf "a\nb\n") | tr -d " "'
# a space still means a redirection of a subshell, so these must NOT change
check bx-redir-plain    'ok'            0 'echo a > /dev/null; echo ok'
check bx-redir-in       'ok'            0 'cat < /etc/hosts | head -1 >/dev/null; echo ok'

# --- bash extensions (tranche 10: coproc, subscripts, SIGPIPE) -------------
# coproc: two pipes, NAME[0] reads its output, NAME[1] writes to its input
check bx-coproc         'ok'            0 'coproc c { echo hi; }; echo ok'
check bx-coproc-fds     'n=2'           0 'coproc { echo hi; }; echo "n=${#COPROC[@]}"'
check bx-coproc-read    'l=hello'       0 'coproc MY { echo hello; }; read -u ${MY[0]} l; echo "l=$l"'
check bx-coproc-pid     'haspid'        0 'coproc MY { echo x; }; [ -n "$MY_PID" ] && echo haspid'
check bx-coproc-two     'ab'            0 'coproc C { echo a; echo b; }; read -u ${C[0]} x; read -u ${C[0]} y; echo "$x$y"'
# a subscript may contain whitespace; `echo [a b]' must stay two words
check bx-sub-space      'v'             0 'declare -A m; m[a b]=v; echo "${m[a b]}"'
check bx-sub-space-two  'vw'            0 'declare -A m; m[a b]=v; m[c d]=w; echo "${m[a b]}${m[c d]}"'
check bx-sub-space-app  'xy'            0 'declare -A m; m[a b]+=x; m[a b]+=y; echo "${m[a b]}"'
check bx-sub-arith      'x'             0 'a=(0); a[1 + 1]=x; echo "${a[2]}"'
check bx-glob-not-sub   '[a b]'         0 'echo [a b]'
# SIGPIPE: a producer outliving its consumer dies quietly, status 141
check bx-sigpipe-quiet  'y
y'                                      0 'yes | head -2'
check bx-sigpipe-status '141'           0 'yes 2>/dev/null | head -1 >/dev/null; echo ${PIPESTATUS[0]}'
check bx-sigpipe-seq    '1
2'                                      0 'seq 1 100000 2>/dev/null | head -2'

# --- history and fc -------------------------------------------------------
# POSIX requires `fc'; it is meaningless without a history list. These cover
# the non-interactive paths -- recording only happens at a real prompt, so the
# interactive behaviour is tested in test/history-pty.py instead.
check hist-empty        ''              0 'history'
check_err fc-empty      'history is empty' 1 'fc -l'
check hist-clear        ''              0 'history -c'
# a script must not accumulate history: recording is REPL-only
check hist-not-in-script '0'            0 'echo a >/dev/null; echo b >/dev/null; history | wc -l | tr -d " "'
# HISTSIZE=0 disables recording entirely
check hist-size-zero    ''              0 'HISTSIZE=0; history'
# the non-interactive path must be untouched by any of this
check noninteractive-c  'hi'            0 'echo hi'

# --- stdin / REPL mode ----------------------------------------------------
got=$(printf 'echo from-stdin\nexit 0\n' | "$SXSH" 2>/dev/null)
if printf '%s' "$got" | grep -q 'from-stdin'; then
  pass=$((pass + 1))
else
  fail=$((fail + 1))
  printf 'FAIL stdin-mode\n  want output containing from-stdin\n  got: %q\n' "$got"
fi

printf '\nsmoke: %d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]

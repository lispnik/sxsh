#!/usr/bin/env bash
# posix-diff.sh --- differential conformance suite.
#
# Runs the same source through sxsh and a reference POSIX shell and compares
# (stdout+stderr, exit status). This catches whole classes of divergence that
# hand-written expectations miss, because the reference supplies the expected
# answer instead of us guessing it.
#
#   ./test/posix-diff.sh [path-to-sxsh] [reference-shell]
#
# Reference defaults to bash; anything POSIX-conforming works (dash, ksh).
# Cases where shells legitimately disagree are listed in KNOWN_DIVERGENCE
# below with the reason, and are reported but not counted as failures.

set -u

SXSH=${1:-./sxsh}

# Pick a modern bash by preference. macOS still ships 3.2.57 (2007) as
# /bin/bash, which predates features the standard has since adopted -- $'\u'
# among them -- so using it as the reference invents divergences that say
# nothing about our conformance.
pick_ref() {
  local c
  for c in /opt/homebrew/bin/bash /usr/local/bin/bash /bin/bash; do
    [ -x "$c" ] && { printf '%s' "$c"; return; }
  done
  printf '%s' /bin/bash
}
REF=${2:-$(pick_ref)}

pass=0; fail=0; skipped=0

run() { # run <shell> <src> -> prints "status<US>output"
  local out st
  out=$("$1" -c "$2" 2>&1); st=$?
  printf '%s\037%s' "$st" "$out"
}

# probe <name> <src> [why-divergence-is-allowed]
#
# With a third argument the case is expected to differ in *text* only; we then
# require the exit STATUS to match and ignore the message. That keeps the case
# meaningful (a wrong status still fails) instead of skipping it outright.
# Allowed reasons so far:
#   diag-text   POSIX does not specify shell diagnostic wording; bash says
#               "bash: line N: ...", we print our own.
#   dollar-zero $0 for -c is implementation-defined.
probe() {
  local name=$1 src=$2 why=${3:-} a b
  a=$(run "$SXSH" "$src")
  b=$(run "$REF" "$src")
  if [ "$a" = "$b" ]; then
    pass=$((pass + 1))
    return
  fi
  if [ -n "$why" ] && [ "${a%%$'\037'*}" = "${b%%$'\037'*}" ]; then
    skipped=$((skipped + 1))          # same status, wording differs: allowed
    return
  fi
  fail=$((fail + 1))
  printf 'FAIL %s%s\n  src:  %s\n  sxsh: %s\n  ref:  %s\n' \
    "$name" "${why:+ (expected only $why to differ)}" "$src" \
    "$(printf '%s' "$a" | tr '\037\n' '|/')" \
    "$(printf '%s' "$b" | tr '\037\n' '|/')"
}

[ -x "$SXSH" ] || { echo "posix-diff: no executable at $SXSH" >&2; exit 1; }
[ -x "$REF" ]  || { echo "posix-diff: no reference shell at $REF" >&2; exit 1; }

# --- set -e scoping (POSIX 2.14) -------------------------------------------
probe errexit-plain        'set -e; false; echo S'
probe errexit-andand-first 'set -e; false && true; echo S'
probe errexit-andand-last  'set -e; true && false; echo S'
probe errexit-oror-last    'set -e; false || false; echo S'
probe errexit-oror-ok      'set -e; false || true; echo S'
probe errexit-chain        'set -e; true && false && true; echo S'
probe errexit-if-cond      'set -e; if false; then :; fi; echo S'
probe errexit-if-body      'set -e; if true; then false; fi; echo S'
probe errexit-elif-cond    'set -e; if false; then :; elif false; then :; fi; echo S'
probe errexit-while-cond   'set -e; while false; do :; done; echo S'
probe errexit-until-cond   'set -e; until true; do :; done; echo S'
probe errexit-bang-true    'set -e; ! true; echo S'
probe errexit-bang-false   'set -e; ! false; echo S'
probe errexit-pipe-early   'set -e; false | true; echo S'
probe errexit-pipe-last    'set -e; true | false; echo S'
probe errexit-subshell     'set -e; (false); echo S'
probe errexit-brace        'set -e; { false; }; echo S'
probe errexit-function     'set -e; f() { false; }; f; echo S'
probe errexit-for-body     'set -e; for i in 1; do false; done; echo S'
probe errexit-case-body    'set -e; case x in x) false;; esac; echo S'
probe errexit-nested-cond  'set -e; if ! false; then echo S; fi'

# --- read and IFS field splitting (POSIX 2.6.5) ----------------------------
probe read-ifs-colon    'echo "a:b:c" | { IFS=: read x y z; echo "$x-$y-$z"; }'
probe read-ifs-remain   'echo "a:b:c:d" | { IFS=: read x y; echo "[$x][$y]"; }'
probe read-ifs-empty    'echo "a::b" | { IFS=: read x y z; echo "[$x][$y][$z]"; }'
probe read-ifs-leading  'echo ":a" | { IFS=: read x y; echo "[$x][$y]"; }'
probe read-ws-trim      'echo "  a  b  " | { read x y; echo "[$x][$y]"; }'
probe read-single-name  'echo "a b c" | { read x; echo "[$x]"; }'
probe read-short-line   'printf "a\n" | { IFS=: read x y; echo "[$x][$y]"; }'
probe read-mixed-ifs    'echo "a:b c:d" | { IFS=": " read p q r; echo "[$p][$q][$r]"; }'
probe read-rest-to-last 'echo "one two three" | { read a b; echo "[$a][$b]"; }'
probe read-raw          'printf "a\\\\b\n" | { IFS= read -r x; echo "[$x]"; }'
probe read-eof-status   'printf "" | { read x; echo "st=$?"; }'

# --- EXIT trap and exit status --------------------------------------------
probe trap-exit-keeps    'trap "echo t" EXIT; exit 3'
probe trap-exit-zero     'trap "echo t" EXIT; exit 0'
probe trap-exit-override 'trap "exit 7" EXIT; exit 3'
probe trap-exit-last-cmd 'trap "echo t" EXIT; false'
probe trap-exit-implicit 'trap "echo t" EXIT; true'
probe exit-plain         'exit 5'

# --- command substitution runs exactly once --------------------------------
probe cmdsub-once-ext    '/bin/echo $(echo S >&2; echo hi)'
probe cmdsub-once-pipe   '/bin/echo $(echo S >&2; echo hi) | cat'
probe cmdsub-once-concat '/bin/echo a$(echo S >&2)b'
probe cmdsub-once-builtin 'echo $(echo S >&2; echo hi)'
probe cmdsub-nested      'echo $(echo $(echo deep))'

# --- command not found -----------------------------------------------------
probe notfound-status    'nosuchcmd_xyz; echo $?' diag-text
probe notfound-quiet     'nosuchcmd_xyz 2>/dev/null; echo $?'
probe notfound-pipe      'nosuchcmd_xyz | cat; echo "st=$?"' diag-text
probe notfound-pipe-tail 'echo hi | nosuchcmd_xyz; echo "st=$?"' diag-text
probe notfound-diag      'nosuchcmd_xyz' diag-text
probe unset-func-gone    'f() { echo hi; }; unset -f f; f 2>/dev/null; echo $?'

# --- arithmetic ------------------------------------------------------------
probe arith-div0      'echo $((1/0))' diag-text
probe arith-mod0      'echo $((5%0))' diag-text
probe arith-basic     'echo $((2**0 + 6 / 2 * 4))'
probe arith-prec      'echo $((7/2)) $((7%3)) $((1+2*3))'
probe arith-assign    'echo $((x=5, x*x))'
probe arith-ternary   'echo $((1 ? 20 : 30))'
probe arith-octal     'echo $((010))'
probe arith-hex       'echo $((0x10))'

# --- parameter expansion ---------------------------------------------------
probe param-default   'echo ${U:-d}'
probe param-assign    'echo ${U:=dflt}$U'
probe param-length    'V=abcd; echo ${#V}'
probe param-prefix    'F=a.b.c; echo "${F#*.}|${F##*.}"'
probe param-suffix    'F=a.b.c; echo "${F%.*}|${F%%.*}"'
probe param-at-empty  'set --; echo "[$@]" $#'
probe param-star-ifs  'set -- a b; IFS=-; echo "$*"'
probe param-at-split  'set -- "a b" c; for w in "$@"; do echo "[$w]"; done'
probe nounset-unset   'set -u; echo $#'

# --- status propagation ----------------------------------------------------
probe status-bang     '! false; echo $?'
probe status-pipeline 'false | true; echo $?'
probe status-subshell '(exit 4); echo $?'
probe status-func     'f() { return 5; }; f; echo $?'
probe status-empty-if 'if :; then :; fi; echo $?'

# --- redirection -----------------------------------------------------------
probe redir-fd-dup    'exec 3>&1; echo x >&3; exec 3>&-; echo done'
probe redir-heredoc   'X=v; cat <<EOF
[$X]
EOF'
probe redir-heredoc-q "cat <<'EOF'
\$notexpanded
EOF"
probe redir-append    'f=$(mktemp); echo a >"$f"; echo b >>"$f"; cat "$f"; rm -f "$f"'
probe redir-noclobber 'echo hi > /dev/null; echo $?'

# --- misc ------------------------------------------------------------------
probe glob-nomatch    'echo /nonexistent-dir-xyz/*'
probe case-quoted     'case "a b" in "a b") echo q;; esac'
probe backslash-nl    'echo a\
b'
probe command-v       'command -v echo'
probe times-ok        'times >/dev/null; echo $?'
probe export-listing  'export FOO=1; export -p | grep -c FOO'
probe dollar-zero-c   'echo $0' dollar-zero
# NOTE: no alias probe here. bash disables alias expansion in
# non-interactive shells (its own documented deviation, see shopt
# expand_aliases), so it cannot serve as a reference for POSIX alias
# behaviour. sxsh's alias handling is asserted in smoke.sh instead.

# --- printf: flags, width, precision, escapes (POSIX 2.14) ----------------
probe printf-width      'printf "%5d|\\n" 42'
probe printf-left       'printf "%-5d|\\n" 42'
probe printf-zeropad    'printf "%05d\\n" 42'
probe printf-precision  'printf "%.2s\\n" abcdef'
probe printf-star-width 'printf "%*d|\\n" 5 42'
probe printf-b-escapes  'printf "%b\\n" "a\\tb"'
probe printf-octal      'printf "\\101\\n"'
probe printf-dashdash   'printf -- "%s\\n" x'
probe printf-recycle    'printf "%s\\n" a b c'
probe printf-bases      'printf "%x %X %o\\n" 255 255 8'
probe printf-char       'printf "%c" abc; echo'
probe printf-missing    'printf "%s %s\\n" a'
probe printf-float      'printf "%.2f\\n" 3.14159'
probe printf-float-pad  'printf "%05.2f\\n" 1'
probe printf-badnum     'printf "%d\\n" abc' diag-text
probe printf-escapes    'printf "a\\tb\\n"'

# --- test / [ -------------------------------------------------------------
probe test-isatty       '[ -t 0 ]; echo $?'
probe test-noninteger   '[ abc -eq 1 ] 2>/dev/null; echo $?'
probe test-readable     '[ -r /etc/hosts ]; echo $?'
probe test-writable     '[ -w /etc/hosts ]; echo $?'
probe test-executable   '[ -x /bin/sh ]; echo $?'
probe test-not-exec     '[ -x /etc/hosts ]; echo $?'
probe test-symlink      '[ -L /etc ]; echo $?'
probe test-empty-int    '[ "" -eq 1 ] 2>/dev/null; echo $?'
probe test-string-ops   '[ a = a ] && [ a != b ] && [ -n x ] && [ -z "" ]; echo $?'
probe test-numeric      '[ 1 -ne 2 ] && [ 2 -gt 1 ] && [ 1 -le 1 ]; echo $?'

# --- getopts --------------------------------------------------------------
probe getopts-loop      'set -- -a -b arg x; while getopts "ab:" o; do echo "$o=$OPTARG"; done; echo "ind=$OPTIND"'
probe getopts-clustered 'set -- -ab arg; while getopts "ab:" o; do echo "$o=$OPTARG"; done'
probe getopts-end       'set -- x -a; getopts "a" o; echo "st=$? o=$o"'
probe getopts-silent    'set -- -z; getopts ":ab" o; echo "o=$o arg=$OPTARG"'
probe getopts-ddash     'set -- -a -- -b; while getopts "ab" o; do echo "$o"; done; echo "ind=$OPTIND"'

# --- redirections on compound commands (POSIX 2.9.4) ----------------------
probe heredoc-while  'while read l; do echo "[$l]"; done <<EOF
a
b
EOF'
probe heredoc-for    'for i in 1; do read l; echo "[$l]"; done <<EOF
y
EOF'
probe heredoc-if     'if read l; then echo "[$l]"; fi <<EOF
z
EOF'
probe heredoc-case   'case x in x) read l; echo "[$l]";; esac <<EOF
c
EOF'
probe heredoc-brace  '{ read l; echo "[$l]"; } <<EOF
x
EOF'
probe heredoc-subsh  '( read l; echo "[$l]" ) <<EOF
w
EOF'
probe heredoc-tabs   'cat <<-EOF
	tabbed
	EOF'
probe heredoc-two    'cat <<E1; cat <<E2
one
E1
two
E2'
probe heredoc-pipe   'cat <<EOF | tr a-z A-Z
lower
EOF'
probe heredoc-arith  'cat <<EOF
$((1+2))
EOF'
probe heredoc-in-fn  'f() { cat <<EOF
in-func
EOF
}; f'

# --- fd manipulation ------------------------------------------------------
probe fd-dup-out     'exec 3>&1; echo x >&3; exec 3>&-; echo done'
probe fd-read-from   'exec 4</etc/hosts; read l <&4; exec 4<&-; [ -n "$l" ] && echo got'
probe fd-close-unused 'exec 5>&-; echo ok'
probe fd-swap        'echo x 1>&2 2>&1 | cat'
probe fd-order       '{ echo a; } 2>&1 1>/dev/null; echo done'
probe redir-missing-in  'cat < /nonexistent-zz 2>/dev/null; echo st=$?' diag-text
probe redir-missing-msg 'cat < /nonexistent-zz' diag-text
probe redir-bad-outdir  'echo x > /nonexistent-dir-zz/f' diag-text

# --- control-flow edges ---------------------------------------------------
probe for-empty      'for i in; do echo $i; done; echo empty-ok'
probe break-nested   'for i in 1 2; do for j in a b; do break 2; done; done; echo ok'
probe continue-2     'for i in 1 2; do for j in a b; do [ $j = b ] && continue 2; echo "$i$j"; done; done'
probe shift-too-many 'set -- a; shift 2 2>/dev/null; echo st=$?'
probe func-nested    'f() { g() { echo nested; }; }; f; g'
probe return-bare    'f() { return; }; f; echo $?'

# --- parameter expansion corners (POSIX 2.6.2) ----------------------------
probe pe-unset-dash     'unset x; echo "${x-def}"'
probe pe-null-dash      'x=; echo "${x-def}"'
probe pe-null-colondash 'x=; echo "${x:-def}"'
probe pe-assign         'unset x; echo "${x:=set}$x"'
probe pe-alt            'x=v; echo "${x:+alt}"'
probe pe-alt-null       'x=; echo "${x+alt}"'
# NOTE: no ${x:?} probe. POSIX requires a non-interactive shell to exit with
# a non-zero status but does not specify which; bash uses 127, we and zsh use
# 1. The behaviour that IS specified is asserted in smoke.sh.
probe pe-strip-short    'f=a.b.c; echo "${f#*.}"'
probe pe-strip-long     'f=a.b.c; echo "${f##*.}"'
probe pe-suffix-short   'f=a.b.c; echo "${f%.*}"'
probe pe-suffix-long    'f=a.b.c; echo "${f%%.*}"'
probe pe-empty-pattern  'f=abc; echo "${f#}"'
probe pe-nomatch        'f=abc; echo "${f#x}"'
probe pe-basename       'p=/a/b/c; echo "${p##*/}"'
probe pe-dirname        'p=/a/b/c; echo "${p%/*}"'
probe pe-length         'x=abc; echo ${#x}'
probe pe-argc           'set -- a b; echo ${#}'
probe pe-adjacent       'x=a; echo "${x}b"'
probe pe-nounset-dash   'unset u; set -u; echo "${u-ok}"'
probe pe-undef-concat   'echo ${undefined_zz}extra'

# --- quoting --------------------------------------------------------------
probe q-dquote-escape   'echo "a\"b"'
probe q-dollar-escape   'echo "a\$b"'
probe q-backslash       'echo "a\\b"'
probe q-escaped-space   'echo a\ b'
probe q-at-expands      'set -- "a b" c; for i in "$@"; do echo "[$i]"; done'
probe q-at-empty        'set -- ; for i in "$@"; do echo no; done; echo empty'
probe q-at-in-word      'set -- a b; echo "x${@}y"'
probe q-star-ifs        'set -- a b; IFS=-; echo "$*"'
probe q-nested-cmdsub   'echo "$(echo "nested \"quotes\"")"'
probe q-adjacent        'echo a"b"c'

# --- exit status propagation ----------------------------------------------
probe st-subshell       '(exit 3); echo $?'
probe st-func-return    'f() { return 4; }; f; echo $?'
probe st-func-last      'f() { false; }; f; echo $?'
probe st-cmdsub-assign  'x=$(exit 6); echo $?'
probe st-pipeline-last  'true | false; echo $?'
probe st-pipeline-first 'false | true; echo $?'
probe st-negate         '! true; echo $?'
probe st-if-nomatch     'if false; then :; fi; echo $?'
probe st-for-empty      'for i in; do :; done; echo $?'
probe st-case-nomatch   'case x in y) false;; esac; echo $?'
probe st-eval-false     'eval "false"; echo $?'
probe st-eval-empty     'eval ""; echo $?'
probe st-assign-cmdsub  'v=$(true) w=$(false); echo $?'

# --- $'...' C-style quoting (POSIX Issue 8) --------------------------------
probe dq-tab            "echo \$'a\\tb'"
probe dq-newline        "echo \$'l1\\nl2'"
probe dq-hex            "echo \$'\\x41'"
probe dq-octal          "echo \$'\\101'"
probe dq-unicode        "echo \$'\\u0041'"
probe dq-escaped-quote  "echo \$'q:\\'x'"
probe dq-backslash      "echo \$'bs:\\\\'"
probe dq-empty          "echo \$''"
probe dq-no-expansion   "x=v; echo \$'\$x'"
probe dq-one-word       "for w in \$'a b' c; do echo \"[\$w]\"; done"
probe dq-in-assignment  "x=\$'a\\tb'; echo \"[\$x]\""
probe dq-quoted-literal "echo \"\$'literal'\""

# --- ulimit ---------------------------------------------------------------
probe ulimit-default    'ulimit'
probe ulimit-f          'ulimit -f'
probe ulimit-n          'ulimit -n'
probe ulimit-core       'ulimit -c'
probe ulimit-stack      'ulimit -s'
probe ulimit-hard       'ulimit -H -n'
probe ulimit-soft       'ulimit -S -n'
probe ulimit-set        'ulimit -n 256; ulimit -n'
probe ulimit-set-f      'ulimit -f 100; ulimit -f'
probe ulimit-builtin    'command -v ulimit'
probe ulimit-bad-opt    'ulimit -Z 2>/dev/null; echo st=$?'

printf '\nposix-diff: %d passed, %d failed, %d known divergences (ref: %s)\n' \
  "$pass" "$fail" "$skipped" "$("$REF" --version 2>/dev/null | head -1 || echo "$REF")"
[ "$fail" -eq 0 ]

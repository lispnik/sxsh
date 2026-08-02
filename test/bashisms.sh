#!/usr/bin/env bash
# bashisms.sh --- scoreboard for bash extensions, which are NOT POSIX.
#
#   ./test/bashisms.sh [path-to-sxsh] [reference-bash]
#
# sxsh targets POSIX first; everything here is deliberately outside that. The
# goal is to support them IN ADDITION to POSIX, so this file tracks which are
# done. Each case runs under sxsh and under bash and compares output+status,
# exactly like test/posix-diff.sh -- the expectation comes from bash rather
# than from our own assumptions about what bash does. That matters: `RANDOM=5'
# SEEDS bash's generator rather than assigning to it, which is not what you
# would guess.
#
# This is a scoreboard, not a gate. `make bashisms' never fails the build.

set -u
SXSH=${1:-./bin/sxsh}
REF=${2:-}

# Prefer a modern bash: macOS /bin/bash is 3.2 and lacks ${x^^} entirely.
if [ -z "$REF" ]; then
  for c in /opt/homebrew/bin/bash /usr/local/bin/bash /bin/bash; do
    [ -x "$c" ] && REF=$c && break
  done
fi
[ -x "$SXSH" ] || { echo "bashisms: no executable at $SXSH (run: make build)" >&2; exit 1; }
[ -n "$REF" ] || { echo "bashisms: no reference bash found" >&2; exit 1; }

have=0; miss=0; missing=''
c() { # c <name> <source>
  local name=$1 src=$2 a b
  a=$(timeout 5 "$SXSH" -c "$src" 2>&1 | tr '\n' '/')
  b=$(timeout 5 "$REF"  -c "$src" 2>&1 | tr '\n' '/')
  if [ "$a" = "$b" ]; then
    have=$((have + 1))
  else
    miss=$((miss + 1)); missing="$missing  $name\n"
    printf '  MISS %-24s want=[%s] got=[%s]\n' \
      "$name" "$(printf %s "$b" | cut -c1-24)" "$(printf %s "$a" | cut -c1-24)"
  fi
}

echo "== conditional expressions =="
c '[[ ]] basic'      '[[ 1 == 1 ]] && echo y'
c '[[ ]] regex'      '[[ abc =~ ^a ]] && echo y'
c '[[ ]] -v'         'x=1; [[ -v x ]] && echo y'
c '(( )) command'    '((1+1)) && echo y'

echo "== arrays =="
c 'array assign'     'a=(1 2 3); echo ${a[1]}'
c 'array all'        'a=(1 2 3); echo "${a[@]}"'
c 'array length'     'a=(1 2 3); echo ${#a[@]}'
c 'array append'     'a=(1); a+=(2); echo "${a[*]}"'
c 'assoc array'      'declare -A m; m[k]=v; echo ${m[k]}'
c 'PIPESTATUS'       'false | true; echo ${PIPESTATUS[0]}'
c 'read -a'          'echo "1 2 3" | { read -a arr; echo ${arr[1]}; }'
c 'mapfile'          'printf "a\nb\n" | { mapfile arr; echo ${#arr[@]}; }'

echo "== parameter expansion =="
c 'subst first'      'v=aaa; echo ${v/a/b}'
c 'subst all'        'v=aaa; echo ${v//a/b}'
c 'subst anchored'   'v=aaa; echo ${v/#a/X}'
c 'upper ^^'         'v=ab; echo "${v^^}"'
c 'lower ,,'         'v=AB; echo "${v,,}"'
c 'indirect ${!x}'   'x=y; y=z; echo ${!x}'
c 'prefix ${!p*}'    'pfxa=1; pfxb=2; echo ${!pfx*}'
c 'substring'        'v=abcdef; echo ${v:1:3}'

echo "== words and redirection =="
c 'here-string'      'cat <<<hello'
c 'brace list'       'echo {a,b,c}'
c 'brace range'      'echo {1..5}'
c 'brace range step' 'echo {0..10..5}'
c 'process sub'      'cat <(echo hi)'
c '&> redirect'      'echo hi &>/dev/null; echo ok'
c '&>> redirect'     'echo hi &>>/dev/null; echo ok'
c '|& pipe'          'echo hi |& cat'
# Via [[ ]], whose operands are matched at RUNTIME. The `case' form cannot be
# used here: bash parses the whole -c line before the shopt runs, so it is a
# syntax error THERE too -- the case would be testing bash's parse order
# rather than extglob. (sxsh accepts it; see CLAUDE.md.)
c 'extglob'          'shopt -s extglob; v=abc; [[ $v == ab?(c) ]] && echo y'

echo "== syntax =="
c 'function keyword' 'function f { echo hi; }; f'
c 'for ((;;))'       'for ((i=0;i<3;i++)); do printf %s $i; done; echo'
c 'select'           'echo 1 | select x in a b; do echo $x; break; done'
c 'case ;& '         'case a in a) echo 1;& b) echo 2;; esac'
c 'case ;;&'         'case a in a) echo 1;;& a*) echo 2;; esac'
c 'coproc'           'coproc c { echo hi; }; echo ok'

echo "== variables and options =="
c 'name+='           'v=a; v+=b; echo $v'
c 'pipefail'         'set -o pipefail; false | true; echo $?'
c 'RANDOM'           'test "$RANDOM" -ge 0 && echo y'
c 'SECONDS'          'test "$SECONDS" -ge 0 && echo y'
c 'LINENO'           'echo $LINENO'
c 'shopt'            'shopt -s nullglob && echo ok'
c 'declare -i'       'declare -i n; n=3+4; echo $n'
c 'local -i'         'f() { local -i n=2+2; echo $n; }; f'
c 'nameref local -n' 'f() { local -n r=$1; echo $r; }; v=hi; f v'

echo "== builtins =="
c 'read -n'          'printf 12345 | { read -n 3 x; echo $x; }'
c 'read -d'          'printf a:b | { read -d : x; echo $x; }'
c 'read -t'          'read -t 0 </dev/null; echo $?'
c 'read -u'          'read -u 3 3<<<hi; echo $REPLY'
c 'read -p'          'echo x | { read -p "" v; echo $v; }'
c 'printf -v'        'printf -v v %s hi; echo $v'
c 'printf %q'        'printf "%q\n" "a b"'
c 'printf %*d'       'printf "%*d|\n" 5 42'
c 'type -a'          'type -a echo >/dev/null; echo ok'
c 'echo -e'          'echo -e "a\tb"'
c 'trap ERR'         'trap "echo E" ERR; false; echo done'
c 'trap DEBUG'       'trap "" DEBUG; echo ok'
# The elapsed times obviously differ run to run, so compare only the SHAPE of
# the output: three lines named real/user/sys, plus the command's own status.
c 'time keyword'     '{ time true; } 2>&1 | grep -c "^real\|^user\|^sys"'

printf '\nbashisms: %d supported, %d missing (ref: %s)\n' \
  "$have" "$miss" "$("$REF" --version 2>/dev/null | head -1)"
exit 0

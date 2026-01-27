#!/bin/sh
# A fake REPL that pretends to be GHCi, for cape-tidal's integration tests.
# Emit the "tidal> " prompt in the same form as the real one (no newline,
# trailing space).
printf 'tidal> '
while IFS= read -r line; do
  case "$line" in
    ":complete repl"*)
      printf '3 3 ""\n"stut"\n"stutter"\n"stutWith"\n'
      ;;
    ":type"*)
      printf 'it :: Pattern String\n'
      ;;
    slow*)
      sleep 3
      printf 'slow done\n'
      ;;
    d1*)
      printf 'played\n'
      ;;
    "")
      ;;
    *)
      printf 'unknown\n'
      ;;
  esac
  printf 'tidal> '
done

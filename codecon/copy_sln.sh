#! /bin/sh
[ "$#" -eq 1 ] || exit 1

if cp "$1".cc "sln/"; then
  rm -r "$1"*
  git add sln/"$1".cc
  echo "$1 copied and added"
fi

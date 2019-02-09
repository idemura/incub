#! /bin/sh
[ "$#" -eq 1 ] || exit 1

if cp "$1".cc "sln/"; then
  rm -r "$1"*
  git add sln/"$1".cc
  git commit -m "$1"
fi

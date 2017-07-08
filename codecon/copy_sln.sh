#! /bin/sh
if [ "$#" -ne 1 ]; then
  exit -1
fi

if cp "$1".cc "sln/"; then
  rm -r "$1"*
  git add sln/"$1".cc
  echo "$1 copied and added"
fi


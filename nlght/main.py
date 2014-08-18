import cmdline
import compile
import os

VERSION = '0.0.0'

def main(argv):
  cmdline.parse(argv[1:])
  if len(cmdline.positional) == 0:
    print 'Usage: nlght <file>'
    print 'Version: {0}'.format(VERSION)
    return -1
  for file_name in cmdline.positional:
    with open(file_name, 'rt') as f:
      compile.compile(file_name, f.readlines())
  # Error means program will exit in when error detected.
  return 0

if __name__ == '__main__':
  from sys import argv
  exit(main(argv))

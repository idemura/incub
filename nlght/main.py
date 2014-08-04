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
  print cmdline.kvalue
  with open(cmdline.positional[0], 'rt') as f:
    compile.compile(f.readlines())
    # Dump compiled to C file and run C compiler (?).
  return 0

if __name__ == '__main__':
  from sys import argv
  exit(main(argv))

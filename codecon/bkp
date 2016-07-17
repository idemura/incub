#!/usr/bin/python

import glob
import os
import os.path
import re
import sys
import shutil

HELP = """Usage:
bkp <name>
  Save file <name> to backup
bkp <name> --pop|-p
  Restore latest version of <name> from backup

Flags:
  -q  Quiet, no stdout messages

To clean all backups, run 'rm -rf .bkp'
"""
BKP_DIR = '.bkp'

# Returns a map from version to backup file name.
def get_latest_backup(name):
  r = re.compile(name + '\.(\d+)\.bkp')
  latest_v = -1
  latest = None
  for bkp in glob.glob(os.path.join(BKP_DIR, name + '*.bkp')):
    m = r.search(bkp)
    if m is not None:
      v = int(m.group(1))
      if v > latest_v:
        latest_v = v
        latest = bkp
  return latest_v, latest

def backup_name(name, n):
  return os.path.join(BKP_DIR, name + '.{0}.bkp'.format(n))

def parse_cmdline(args):
  pos = []
  flags = {'-p': False, '-q': False}
  for a in args:
    if a == '--pop':
      flags['-p'] = True
    elif a == '-p' or a == '-q':
      flags[a] = True
    else:
      pos.append(a)
  return pos, flags

def run(args):
  if len(args) == 0:
    print HELP
    return 0

  f, flags = parse_cmdline(args)
  if len(f) != 1:
    print HELP
    return -1

  bkp_v, bkp = get_latest_backup(f[0])
  if flags['-p']:
    shutil.move(bkp, f[0])
  else:
    shutil.copy(f[0], backup_name(args[0], bkp_v + 1))
  return 0

try:
  os.makedirs(BKP_DIR)
except OSError:
  pass

sys.exit(run(sys.argv[1:]))


#! /usr/bin/python

from __future__ import print_function
import glob
import os
import stat
import sys

def glob_remove(pat):
  for f in glob.glob(pat):
    os.remove(f)

glob_remove('*.o')
glob_remove('*.dSYM')

white_list = ['add_sln', 'bkp', 'build', 'clean']
for dir, subdir_list, file_list in os.walk('.'):
  for f in file_list:
    fpath = os.path.join(dir, f)
    if os.stat(fpath).st_mode & stat.S_IEXEC:
      if f not in white_list:
        print('Removing', fpath)
        os.remove(fpath)


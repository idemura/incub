#! /usr/bin/python

from __future__ import print_function

import glob
import os
import shutil
import stat
import sys


do_rm = False
for a in sys.argv[1:]:
    if a == '--rm':
        do_rm = True


def remove(f):
    if do_rm:
        if os.path.isdir(f):
            shutil.rmtree(f)
        else:
            os.remove(f)
    else:
        print(f)


def glob_remove(pat):
    for f in glob.glob(pat):
        remove(f)


def remove_executables(directory, white_list=None):
    white_list = white_list or []
    for f in os.listdir(directory):
        if not os.path.isdir(f) and f not in white_list:
            if os.stat(f).st_mode & stat.S_IEXEC:
                remove(f)


glob_remove('*.o')
glob_remove('*.dSYM')
remove_executables('.', white_list=['bkp.py', 'build.sh', 'clean.py'])


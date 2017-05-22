#! /usr/bin/python

from __future__ import print_function

import glob
import os
import shutil
import stat
import sys


def glob_remove(pat):
    for f in glob.glob(pat):
        if os.path.isdir(f):
            shutil.rmtree(f)
        else:
            os.remove(f)


def remove_executables(directory, white_list=None):
    white_list = white_list or []
    for f in os.listdir(directory):
        if not os.path.isdir(f) and f not in white_list:
            if os.stat(f).st_mode & stat.S_IEXEC:
                print('Remove', f)
                os.remove(f)


glob_remove('*.o')
glob_remove('*.dSYM')
remove_executables('.', white_list=['bkp.py', 'build.sh', 'clean.py'])


#! /usr/bin/python

from __future__ import print_function

import argparse
import glob
import os
import shutil
import stat
import sys


parser = argparse.ArgumentParser(description='Remove compiler outputs')
parser.add_argument('--rm', action='store_true', help='actually remove files')
args = parser.parse_args()

def remove(f):
    if args.rm:
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
        _, ext = os.path.splitext(f)
        if not os.path.isdir(f) and f not in white_list and len(ext) == 0:
            if os.stat(f).st_mode & stat.S_IEXEC:
                remove(f)


for p in ['*.a', '.o', '*.dSYM', '*.log']:
    glob_remove(p)
remove_executables('.', white_list=[])


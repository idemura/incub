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
    if os.path.isdir(f):
        shutil.rmtree(f)
    else:
        os.remove(f)


def find_glob(pattern_list):
    res = []
    for pat in pattern_list:
        res += glob.glob(pat)
    return res


def find_binary(directory):
    res = []
    for f in os.listdir(directory):
        if os.path.isfile(f) and len(os.path.splitext(f)[1]) == 0 and \
                (os.stat(f).st_mode & stat.S_IEXEC) != 0:
            res.append(f)
    return res


files = find_glob(['*.a', '*.o', '*.mk', '*.dSYM', '*.log']) + find_binary('.')
if args.rm:
    for f in files:
        remove(f)
else:
    for f in sorted(files):
        print(f)

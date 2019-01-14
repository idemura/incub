#! /usr/bin/python3

import argparse
import os
import platform
import string
import sys

MAKE_TMPL = """CXX=${cxx}
CXX_LANG=-std=c++17 -I. -march=native -fdiagnostics-color=auto -fno-exceptions -fno-rtti ${warnings}
CXX_MODE=${mode}
CXX_LIBS=-lgtest -lgmock -lglog -lgflags -pthread
CXX_TOOL=$$(CXX) $$(CXX_LANG) $$(CXX_MODE) $$(CXX_LIBS)

.PHONY: run

${name}: ${name}.cc
\t$$(CXX_TOOL) $$? -o $$@

run: ${name}
\t./${name} ${in}
"""

CODE_TMPL = """// ${name}
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <map>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#include "log.h"

using namespace std;

using i32 = int32_t;
using i64 = int64_t;
using pii = std::pair<int, int>;

int main(int argc, char **argv) {
    initLog(argc, argv);
    return 0;
}
"""


def write_template(t, vars, name):
    with open(name, "wt") as f:
        s = string.Template(t).substitute(**vars)
        f.write(s)


def format_warnings(wlist):
    return " ".join(["-W" + w for w in wlist])


def create_makefile(args):
    name = args.name[0]
    vars = {}
    vars["name"] = name
    warnings = ["all", "shadow", "conversion",
            "no-unused-function",
            "no-sign-conversion",
            "no-sign-compare",
            "no-char-subscripts"]
    if platform.system() == "Darwin":
        vars["cxx"] = "clang++"
        warnings.append("literal-range")
    else:
        vars["cxx"] = "g++"
    vars["warnings"] = format_warnings(warnings)
    if args.opt:
        vars["mode"] = "-O3 -ffast-math -flto -DNDEBUG"
    else:
        vars["mode"] = "-O0 -g -fsanitize=address -fno-omit-frame-pointer"
    if args.input:
        vars["in"] = "< " + name + ".in"
    else:
        vars["in"] = ""
    write_template(MAKE_TMPL, vars, name + ".mk")


def create_code(args):
    name = args.name[0]
    vars = {}
    vars["name"] = name
    write_template(CODE_TMPL, vars, name + ".cc")


def create_input(args):
    if args.input:
        name = args.name[0]
        write_template("", {}, name + ".in")


ap = argparse.ArgumentParser(
        description="Generate make files for codecon problems")
ap.add_argument("name", nargs=1, help="target name")
ap.add_argument("-o", "--opt",
        action="store_true",
        help="optimize")
ap.add_argument("-i", "--input",
        action="store_true",
        help="generate input")
args = ap.parse_args()

create_code(args)
create_makefile(args)
create_input(args)

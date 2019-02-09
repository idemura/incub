#! /usr/bin/env python3.6

import argparse
import os
import platform
import string
import sys

MAKEFILE = """\
CXX=${cxx}
CXX_LANG=-std=c++17 -I. -march=native -fdiagnostics-color=auto -fno-exceptions -fno-rtti ${warnings}
CXX_MODE=${mode}
CXX_LIBS=-lgtest -lgmock -lglog -lgflags -pthread
CXX_TOOL=$$(CXX) $$(CXX_LANG) $$(CXX_MODE)

.PHONY: run

${name}: ${name}.cc ${header}
\t$$(CXX_TOOL) $$< -o $$@ $$(CXX_LIBS)

run: ${name}
\t./${name} ${redir_in}
"""

PREAMBLE="""\
#include <algorithm>
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
"""

SOURCE = """\
%header
#include "${name}.h"
%:
${preamble}
%%

int main(int argc, char **argv) {
    initLog(argc, argv);
    return 0;
}
"""

HEADER = """\
#pragma once

${preamble}
"""


def process_if(src, **kw):
    res = ""
    stack = []
    state = True
    for l in src.split("\n"):
        if l.startswith("%"):
            var = l[1:].strip()
            if var == ":":
                # if-else
                if len(stack) == 0:
                    raise NameError("if-stack is empty")
                state = not state
            elif var == "%":
                # if-end
                if len(stack) == 0:
                    raise NameError("if-stack is empty")
                stack = []
                state = True
            else:
                # if-then
                if len(stack) != 0:
                    raise NameError("processing " + stack[0])
                state = var in kw and kw[var]
                stack.append(var)
        elif state:
            res += l + "\n"
    return res


def write_template(t, vars, name, overwrite=False):
    if os.path.exists(name) and not overwrite:
        return
    with open(name, "wt") as f:
        s = string.Template(t).substitute(**vars)
        f.write(s)


def format_warnings(wlist):
    return " ".join(["-W" + w for w in wlist])


def create_makefile(args):
    name = args.name[0]
    vars = {}
    vars["name"] = name
    if args.header:
        vars["header"] = name + ".h"
    else:
        vars["header"] = ""
    warnings = ["all", "shadow",
            "conversion",
            "no-sign-conversion",
            "no-unused-function",
            "no-sign-compare",
            "no-char-subscripts"]
    if platform.system() == "Darwin" or args.clang:
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
        vars["redir_in"] = "< " + name + ".in"
    else:
        vars["redir_in"] = ""
    write_template(MAKEFILE, vars, name + ".mk", overwrite=True)


def create_code(args):
    name = args.name[0]
    vars = {}
    vars["name"] = name
    vars["preamble"] = PREAMBLE.strip()
    write_template(process_if(SOURCE, header=args.header), vars, name + ".cc")
    if args.header:
        write_template(HEADER, vars, name + ".h")


def create_input(args):
    if args.input:
        name = args.name[0]
        write_template("", {}, name + ".in")


ap = argparse.ArgumentParser(
        description="Generate make files for codecon problems")
ap.add_argument("name", nargs=1, help="target name")
ap.add_argument("--clang", action="store_true", help="override to use clang")
ap.add_argument("-o", "--opt",
        action="store_true",
        help="optimize")
ap.add_argument("-i", "--input",
        action="store_true",
        help="generate input")
ap.add_argument("-H", "--header",
        action="store_true",
        help="generate header")
args = ap.parse_args()

create_code(args)
create_makefile(args)
create_input(args)

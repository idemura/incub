from __future__ import print_function
import os
import sys

MAKE_HEADER = '''MODE=dbg
RM=rm -f
AR=ar rcs
CXX=g++-4.9
CC=gcc
ifeq ($(MODE), dbg)
	CXXOPT=-O0 -g
	LDOPT=
else
	CXXOPT=-O3 -ffast-math -DNDEBUG
	LDOPT=-flto
endif
CXXWARNS=-Wall -Wno-unused-function -Wno-sign-compare -Wno-char-subscripts
CXXFLAGS=-std=c++14 -I. -march=native -fdiagnostics-color=auto $(CXXOPT) $(CXXWARNS)
.PHONY: clean
'''

def repl_ext(f, ext):
    return os.path.splitext(f)[0] + ext

def fail(msg):
    sys.stderr.write(msg + '\n')
    sys.exit(-1)

def arg_list(args, prefix, sep):
    s = []
    for a in args:
        s.append(prefix + a)
    return sep.join(s)

def test_name(f):
    return os.path.splitext(f)[0]

def merge(a, b):
    r = a.copy()
    for k in b:
        r[k] = 0
    return r

def lib_full_name(l):
    return 'lib' + l + '.a'

def lib_base_name(l):
    b = os.path.splitext(l)[0]
    return b[3:]

class MakeRule:
    def __init__(self, name, output, deps, cmds):
        self.name = name
        self.output = output
        self.deps = deps
        self.cmds = cmds

    def code(self):
        if self.name is None:
            return ''
        s = self.output + ':'
        if len(self.deps) > 0:
            s += ' ' + ' '.join(self.deps)
        s += '\n'
        for c in self.cmds:
            s += '\t' + c + '\n'
        return s

# Base build target class. Contains dependencies (children).
class Target:
    def __init__(self, deps):
        self.deps = deps
        # *_deps are transitive closures.
        self.lib_deps = None
        self.obj_deps = None
        self.visited = False

    def rule_deps(self):
        rule_deps = []
        for d in self.deps:
            if isinstance(d, TargetSysLib): continue
            rule_deps.append(d.output())
            if isinstance(d, TargetCxxBin):
                fail('Dependency on C++ binary is not allowed')
        return sorted(rule_deps)

# Generic file needs no a build rule.
class TargetFile(Target):
    def __init__(self, file_name):
        Target.__init__(self, [])
        self.file_name = file_name
    def __str__(self):
        return 'TargetFile: file_name=' + self.file_name
    def output(self):
        return self.file_name
    def get_obj(self):
        return []
    def get_lib(self):
        return []
    def generate(self):
        return MakeRule(None, None, [], [])

# Builds C++ binary.
class TargetCxxBin(Target):
    def __init__(self, file_name, deps):
        Target.__init__(self, deps)
        self.file_name = file_name
    def __str__(self):
        return 'TargetCxxBin: file_name=' + self.file_name
    def output(self):
        return self.file_name
    def get_obj(self):
        return []
    def get_lib(self):
        return []
    def generate(self):
        rule_deps = self.rule_deps()
        libs = set()
        for l in self.lib_deps:
            libs.add(lib_base_name(l.output()))
        objs = set()
        for o in self.obj_deps:
            objs.add(o.output())
        cmd = '$(CXX) $(LDOPT) -L. -o {0} {1} {2}'.format(
                self.output(),
                ' '.join(sorted(list(objs))),
                arg_list(sorted(list(libs)), '-l', ' '))
        return MakeRule(self.file_name, self.output(), rule_deps, [cmd])

# Builds C++ file (with possibly .hxx header).
class TargetCxxMod(Target):
    def __init__(self, file_name, deps):
        Target.__init__(self, deps)
        self.file_name = file_name
    def __str__(self):
        return 'TargetCxxMod: file_name=' + self.file_name
    def output(self):
        return repl_ext(self.file_name, '.o')
    def get_obj(self):
        return [self]
    def get_lib(self):
        return []
    def generate(self):
        rule_deps = self.rule_deps()
        hxx = repl_ext(self.file_name, '.hxx')
        if os.path.isfile(hxx):
            rule_deps.append(hxx)
        cmd = '$(CXX) $(CXXFLAGS) -c {0}'.format(self.file_name)
        return MakeRule(self.file_name, self.output(), rule_deps, [cmd])

# Naive build of a C library.
class TargetCLib(Target):
    def __init__(self, name, sources, defines):
        Target.__init__(self, [])
        self.name = name
        self.sources = sources
        self.defines = defines
    def __str__(self):
        return 'TargetCLib: file_name=' + self.name
    def output(self):
        return lib_full_name(self.name)
    def get_obj(self):
        return []
    def get_lib(self):
        return [self]
    def generate(self):
        cmd = []
        obj = []
        for s in self.sources:
            if os.path.splitext(s)[1] == '.c':
                cmd.append(
                    '$(CC) -I. -march=native $(CXXOPT) -c {0} {1}'.format(
                        s, arg_list(self.defines, '-D', ' ')))
                obj.append(repl_ext(s, '.o'))
        cmd.append('$(AR) {0} {1}'.format(self.output(), ' '.join(obj)))
        return MakeRule(self.name, self.output(), self.sources, cmd)

# System library.
class TargetSysLib(Target):
    def __init__(self, name):
        Target.__init__(self, [])
        self.name = name
    def __str__(self):
        return 'TargetSysLib: file_name=' + self.name
    def output(self):
        return lib_full_name(self.name)
    def get_obj(self):
        return []
    def get_lib(self):
        return [self]
    def generate(self):
        return MakeRule(None, None, [], [])

# True if target's output may be deleted.
def do_clean(t):
    return not(isinstance(t, TargetFile) or isinstance(t, TargetSysLib))

# Walks dependency tree and collects libs.
def walk_libs(t):
    if t.lib_deps is not None:
        return t.lib_deps
    t.lib_deps = set(t.get_lib())
    for ct in t.deps:
        t.lib_deps |= walk_libs(ct)
    return t.lib_deps

# Walks dependency tree and collects objs.
def walk_objs(t):
    if t.obj_deps is not None:
        return t.obj_deps
    t.obj_deps = set(t.get_obj())
    for ct in t.deps:
        if isinstance(ct, TargetCxxMod):
            t.obj_deps |= walk_objs(ct)
    return t.obj_deps

# MakeFile class. Use its methods to add targets and generate output file.
# After calling @makefile/@write object state is frozen: you can't add more
# targets into it.
class MakeFile:
    def __init__(self, sys_libs=[]):
        self.targets = {}
        self.tests = []
        for l in sys_libs:
            self.sys_lib(l)
        self.def_rule = None
        self.m = None

    def generic_file(self, file_name):
        if not self.dup_target(file_name):
            self.targets[file_name] = TargetFile(file_name)

    def cxx_bin(self, file_name, deps):
        if not self.dup_target(file_name):
            self.targets[file_name] = TargetCxxBin(file_name, deps)

    def cxx_mod(self, file_name, deps):
        if not self.dup_target(file_name):
            self.targets[file_name] = TargetCxxMod(file_name, deps)

    def cxx_test(self, file_name, deps):
        self.cxx_mod(file_name, deps)
        self.cxx_bin(test_name(file_name), [file_name])
        self.tests.append(file_name)

    def sys_lib(self, name):
        if not self.dup_target(name):
            self.targets[name] = TargetSysLib(name)

    def generic_clib(self, name, sources, defines):
        if not self.dup_target(name):
            self.targets[name] = TargetCLib(name, sources, defines)

    def set_default(self, deps):
        self.def_rule = deps

    def makefile(self):
        if self.m is not None: return self.m
        # Check deps.
        for name in self.targets:
            t = self.targets[name]
            self.walk_target(name, t)
        # Replace deps references from strings to Targets.
        for name in self.targets:
            t = self.targets[name]
            resolved_deps = []
            for d in t.deps:
                resolved_deps.append(self.targets[d])
            t.deps = resolved_deps
        for name in self.targets:
            walk_libs(self.targets[name])
            walk_objs(self.targets[name])
        m = MAKE_HEADER
        if self.def_rule is not None:
            m += 'all:'
            for name in self.def_rule:
                t = self.targets[name]
                if d not in self.targets:
                    fail('Default rule not found ' + name)
                m += ' ' + t.output()
            m += '\n'
        output = []
        for name in self.targets:
            t = self.targets[name]
            r = t.generate()
            m += r.code()
            if do_clean(t):
                output.append(t.output())
            if name in self.tests:
                output.append(test_name(name))
        m += 'clean:\n\t$(RM) {0}\n'.format(' '.join(output))
        if len(self.tests) > 0:
            test_output = []
            for t in self.tests:
                test_output.append(test_name(t))
            m += 'tests: {0}\n'.format(' '.join(test_output))
            for t in test_output:
                m += '\t./{0}\n'.format(t)
            m += '\t@echo TESTS PASSED\n'
        self.m = m
        return m

    def write(self, file_name='Makefile'):
        with open(file_name, 'wt') as f:
            f.write(self.makefile())

    # END PUBLIC METHODS.

    def dup_target(self, file_name):
        if file_name in self.targets:
            fail('Duplicated target ' + file_name)
        return False

    # @name: String
    # @t: Target
    def walk_target(self, name, t):
        if t.visited:
            return
        t.visited = True
        for d in t.deps:
            if d not in self.targets:
                fail('No target: ' + d + ' needed by ' + name)
            self.walk_target(d, self.targets[d])



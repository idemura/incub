import ninja_syntax
import sys

n = ninja_syntax.Writer(open('build.ninja', 'w'))
platform = sys.platform

n.variable('ninja_required_version', '1.3')
n.newline()
n.variable('cflags_dbg', ['-O0', '-g', '-DDEBUG'])
n.variable('cflags_opt', ['-O2', '-flto', '-ffast-math', '-DNDEBUG'])
n.variable('mode', '$cflags_dbg')
n.variable('cflags_nowarnings', [
    '-Wno-unused-function',
    '-Wno-unused-result',
    '-Wno-sign-compare',
    '-Wno-char-subscripts',
    '-Wno-unused-const-variable',
])
n.variable('cflags', [
    '-Wall', '-lm', '-march=native', '$cflags_nowarnings'
])

if (platform.startswith('darwin')):
  cxx = 'clang++'
else:
  cxx = 'g++'
cc = 'gcc'

n.variable('lang_c', ['-fvisibility=hidden'])
n.variable('lang_cxx', [
    '-std=c++0x',
    '-lstdc++',
    '-fno-rtti',
    '-fno-exceptions',
    '-fvisibility=hidden'])
n.newline()

def c_command(lang, lib = False):
  c = lang + ' $cflags $mode -o $out $in'
  if lib: c += ' -c'
  return c

bin_c = 'bin_c'
lib_c = 'lib_c'
bin_cxx = 'bin_cxx'
lib_cxx = 'lib_cxx'

n.rule(bin_c, c_command(cc + ' $lang_c'))
n.rule(lib_c, c_command(cc + ' $lang_c', True))
n.rule(bin_cxx, c_command(cxx + ' $lang_cxx'))
n.rule(lib_cxx, c_command(cxx + ' $lang_cxx', True))
n.newline()

def all_inputs(srcs, deps=[]):
  return srcs + [x + '.o' for x in deps]

def c_library(name, srcs, deps=[]):
  n.build(name + '.o', lib_c, all_inputs(srcs, deps))
def c_binary(name, srcs, deps=[]):
  n.build(name, bin_c, all_inputs(srcs, deps))
def cxx_library(name, srcs, deps=[]):
  n.build(name + '.o', lib_cxx, all_inputs(srcs, deps))
def cxx_binary(name, srcs, deps=[]):
  n.build(name, bin_cxx, all_inputs(srcs, deps))

c_library('mongoose', ['mongoose.c'])
cxx_library('template', ['template.cc'], [])
cxx_binary('main', ['main.cc'], ['mongoose', 'template'])
cxx_binary('template_test', ['template_test.cc'], ['template'])

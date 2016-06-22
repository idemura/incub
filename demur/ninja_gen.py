#!/usr/bin/python

import ninja_syntax
import sys

w = ninja_syntax.Writer(sys.stdout, width=80);

def obj_list(deps):
  r = []
  for d in deps:
    r.append(d + '.o')
  return r

def build_lib(base_name, deps=[]):
  w.build(base_name + '.o',
          'cxx',
          [base_name + '.cxx'])
  w.build(base_name + '.a',
          'lib',
          obj_list([base_name] + deps))

def build_test(base_name, lib=None):
  if lib is None: lib = base_name
  w.build(base_name + '_test.o',
          'cxx',
          [base_name + '_test.cxx'])
  w.build(base_name + '.test',
          'lnk',
          [base_name + '_test.o', lib + '.a'])

def build_bin(base_name):
  w.build(base_name,
          'lnk',
          [base_name + '.a'])

def build_lex(base_name, grammar=None):
  if grammar in None:
    impl = None
  else
    impl = [grammar + '.tab.cxx']
  w.build(base_name + '.yy.cxx',
          'lex',
          [base_name + '.l'],
          implicit=impl)

def build_bison(base_name):
  w.build([base_name + '.tab.cxx', base_name + '.tab.hxx']
          'bison',
          [base_name + '.yxx'])

if len(sys.argv) > 1 and sys.argv[1] == 'opt':
  w.variable('cxx_opt', ['-O3', '-ffast-math', '-DNDEBUG'])
  w.variable('lnk_opt', ['-flto'])
else:
  w.variable('cxx_opt', ['-O0', '-g'])
  w.variable('lnk_opt', [])

w.variable('cxx_bin', 'g++-4.9')
w.variable('cxx_flags',
  [ '-std=c++14',
    '-I.',
    '-march=native',
    '-fdiagnostics-color=always',
    '$cxx_opt',
    '-Wall',
    '-Wno-unused-function',
    '-Wno-sign-compare',
    '-Wno-char-subscripts'])
w.variable('lnk_flags', '$lnk_opt')

w.rule('cxx',
    command='$cxx_bin $cxx_flags -c $in -MMD -MF $out.d',
    depfile='$out.d')
w.rule('lnk',
    command='$cxx_bin $lnk_flags -L. -o $out $in')
w.rule('lib',
    command='ar rcs $out $in')
w.rule('lex',
    command='flex -o $out $in')
w.rule('bison',
    command='bison -d $in')

build_lib('base')
build_test('base')

build_lex('lex', implicit='grammar')
build_lib('lex.yy', ['base'])
build_test('lex', lib='lex.yy')

build_bison('grammar')
build_lib('grammar.tab', ['lex.yy', 'base'])
build_lib('grammar_node', ['lex.yy', 'base'])
build_lib('parser', ['grammar.tab', 'lex.yy', 'base'])
# build lex.yy.o:     cxx lex.yy.cxx
# build lex.yy.a:     lib lex.yy.o base.o
# build lex_test.o:   cxx lex_test.cxx
# build lex.test:     lnk lex_test.o lex.yy.a

# build grammar_node.o:   cxx grammar_node.cxx

# build grammar.tab.cxx:  bison grammar.yxx
# build grammar.tab.o:    cxx grammar.tab.cxx
# build grammar.tab.a:    lib grammar.tab.o lex.yy.o

# build parser.o:         cxx parser.cxx
# build parser.a:         lib parser.o $
#                             grammar_node.o $
#                             grammar.tab.o $
#                             lex.yy.o $
#                             base.o
# build parser_test.o:    cxx parser_test.cxx
# build parser.test:      lnk parser_test.o parser.a

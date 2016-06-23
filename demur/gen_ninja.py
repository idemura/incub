#!/usr/bin/python

import ninja_syntax
import sys

CXX = 'cxx'
LIB = 'lib'
LNK = 'lnk'
LEX = 'lex'
BISON = 'bison'

optimize = len(sys.argv) > 1 and sys.argv[1] == 'opt'
print '# Generated by ninja_gen.py'
if optimize:
  print '# Optimizations ON'
else:
  print '# DEBUG'
w = ninja_syntax.Writer(sys.stdout, width=80);
obj_link = {}
dfs_result_cache = {}

def dfs(node):
  if node in dfs_result_cache:
    return dfs_result_cache[node]
  if node not in obj_link:
    raise Exception('Link not found: ' + l)
  closure = []
  for t in obj_link[node]:
    closure += [t] + dfs(t)
  dfs_result_cache[node] = closure
  return closure

def link_closure(root):
  closure = [root] + dfs(root)
  h = {}
  for l in closure:
    if l in h:
      h[l] += 1
    else:
      h[l] = 1
  r = []
  for l in closure:
    h[l] -= 1
    if h[l] == 0:
      r.append(l + '.o')
  return r

def build_cxx(base_name, link=[]):
  w.build(base_name + '.o',
          CXX,
          base_name + '.cxx')
  if base_name in obj_link:
    raise Exception('Duplicated target ' + base_name)
  obj_link[base_name] = link

def build_bin(base_name, link=[]):
  build_cxx(base_name, link)
  w.build(base_name,
          LNK,
          link_closure(base_name))

def build_lex(base_name, bison=None):
  if bison is None:
    impl = None
  else:
    impl = [bison + '.tab.hxx']
  w.build(base_name + '.yy.cxx',
          LEX,
          base_name + '.l',
          implicit=impl)

def build_bison(base_name):
  w.build([base_name + '.tab.cxx', base_name + '.tab.hxx'],
          BISON,
          [base_name + '.yxx'])

if optimize:
  w.variable('gcc_opt', ['-O3', '-ffast-math', '-DNDEBUG'])
  w.variable('lnk_opt', ['-flto'])
  w.variable('bison_opt', [])
else:
  w.variable('gcc_opt', ['-O0', '-g'])
  w.variable('lnk_opt', [])
  w.variable('bison_opt', ['-t', '-v'])

w.variable('cxx_bin', 'g++-4.9')
w.variable('cxx_warnings',
  [ '-Wall',
    '-Wno-unused-function',
    '-Wno-sign-compare',
    '-Wno-char-subscripts' ])
w.variable('cxx_flags',
  [ '-std=c++14',
    '-I.',
    '-march=native',
    '-fdiagnostics-color=always',
    '$gcc_opt',
    '$cxx_warnings' ])
w.variable('lnk_flags', '$lnk_opt')

w.rule(CXX, command='$cxx_bin $cxx_flags -c $in -MMD -MF $out.d',
            depfile='$out.d')
w.rule(LNK, command='$cxx_bin $lnk_flags -L. -o $out $in')
w.rule(LIB, command='ar rcs $out $in')
w.rule(LEX, command='flex -o $out $in')
w.rule(BISON, command='bison $bison_opt -d $in')

## Build Rules:

build_cxx('base')
build_bin('base_test', ['base'])

build_lex('lex', bison='grammar')
build_cxx('lex.yy', ['grammar_node'])

build_cxx('grammar_node', ['base'])

build_bison('grammar')
build_cxx('grammar.tab', ['lex.yy', 'grammar_node'])
build_cxx('parser', ['grammar_node', 'grammar.tab'])
build_bin('parser_test', ['parser'])

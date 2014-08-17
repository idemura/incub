import re
import itertools as it

class Liner:
  def __init__(self, lines):
    self.lines = lines
    self.line_i = -1

  def getLine(self):
    i = self.line_i + 1
    if i > len(self.lines):
      return None
    l = None
    while i < len(self.lines):
      s = self.lines[i].strip()
      if len(s) > 0 and not s.startswith('##'):
        l = s
        break
      i += 1
    self.line_i = i
    return l

  def moveLineBack(self):
    if self.line_i > 0:
      self.line_i -= 1

class Token:
  def __init__(self, str_rep, name, enum):
    self.str_rep = str_rep
    self.name = name
    self.enum = enum

  def __str__(self):
    return '{name} {str_rep} {enum}'.format(**self.__dict__)

def parseTokens(liner, sec):
  l = liner.getLine()
  e = sec['first']
  tokens = []
  while l is not None:
    if l[0] == '#':
      liner.moveLineBack()
      break
    cols = l.split()
    if len(cols) > 1:
      n = cols[1].upper()
    else:
      n = cols[0].upper()
    tokens.append(Token(cols[0], n, e))
    e += 1
    l = liner.getLine()
  if sec.get('sort'):
    tokens.sort(key=lambda x: x.name)
  return tokens

def parse(lines_in):
  re_section = re.compile(r'#\s*\$\{(\w+)\}')
  sections = {}
  liner = Liner(lines_in)
  l = liner.getLine()
  while l is not None:
    s = {}
    parts = l.split(',')
    m = re_section.match(parts[0])
    for p in parts[1:]:
      columns = p.split()
      name = columns[0]
      if len(columns) == 1:
        s[name] = True
      else:
        s[name] = columns[1]
      if name in 'first':
        s[name] = int(s[name])
    s['tokens'] = parseTokens(liner, s)
    sections[m.group(1)] = s
    l = liner.getLine()
  return sections

def getRegExIndex(lines, re_str, start=0):
  re_obj = re.compile(re_str)
  for i, l in enumerate(lines[start:]):
    m = re_obj.match(l)
    if m is not None:
      return i + start
  return None

def getSectionRangeInOutput(lines, section):
  i0 = getRegExIndex(lines, r'\s*#\s*\$\{' + section + r'\}')
  if i0 is None:
    return (None, None)
  i1 = getRegExIndex(lines, r'\s*#\s*\$END\.', i0 + 1)
  if i1 is None:
    return (None, None)
  return (i0, i1)

def outputSection(lines, sections, sec_name, sec_format):
  lines_out = lines[:]
  i0, i1 = getSectionRangeInOutput(lines_out, sec_name)
  if i0 is None:
    return lines_out
  li0 = lines_out[i0]
  tab = li0[:len(li0) - len(li0.lstrip())]
  lines_out[i0 + 1 : i1] = map(lambda t: tab + sec_format.format(**t.__dict__) + '\n',
                               sections[sec_name]['tokens'])
  return lines_out

def process(name_in, name_out):
  with open(name_in, 'rt') as f:
    lines_in = f.readlines()

  sections = parse(lines_in)
  tokens = []  # All tokens, value sorted.
  for k, sec in sections.iteritems():
    tokens.extend(sec['tokens'])
  tokens.sort(key=lambda x: x.enum)
  sections['Tokens'] = {
    'tokens': tokens
  }

  with open(name_out, 'rt') as f:
    lines_out = f.readlines()

  lines_out = outputSection(lines_out, sections, 'Tokens', "{name} = {enum}")
  lines_out = outputSection(lines_out, sections, 'Keywords', "'{str_rep}' : {name},")
  lines_out = outputSection(lines_out, sections, 'SymOps', "'{str_rep}' : {name},")

  with open(name_out, 'wt') as f:
    f.write(''.join(lines_out))

# Usage:
#   enum.py <in> <out.py>
if __name__ == '__main__':
  from sys import argv
  process(argv[1], argv[2])

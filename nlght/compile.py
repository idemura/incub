import common
import string

class Token:
  EOF = 0
  BEGIN = 1
  END = 2
  LPAREN = 3
  RPAREN = 4
  COMMA = 5

  def __init__(self, type, line, col):
    self.type = type
    self.line = line
    self.col = col

  def __str__(self):
    return 'Token type={0} line={1} col={2}'.format(self.type, self.line,
                                                    self.col)

def getLeftSpaceCount(s):
  i = 0
  while i < len(s) and s[i].isspace():
    i += 1
  return i

def suffixEq(s1, s2):
  ml = min(len(s1), len(s2))
  return s1[:ml] == s2[:ml]

class LineSource
  def __init__(self, lines):
    self.lines = map(string.rstrip, lines)
    self.line_i = 0
    self.indent_stack = [0]
    self.indent = ''

  def getLine(self):
    s = self.lines[self.line_i]
    i = getLeftSpaceCount(s)
    if i == len(s) or s[i] == '#':
      self.line_i += 1
      return getLine()

    indent_s = s[:i]
    if not suffixEq(self.indent, indent_s):
      print ('ET001 {0}@{1}: Line indentation space chars don\'t match ' +
             'to the prevous line').format(self.line_i + 1, i + 1)
      return
    if len(indent_s) > len(self.indent):
      tokens.append(Token(Token.BEGIN, self.line_i, i))
    else:
      while len(indent_s) < self.indent_stack[-1]:
        tokens.append(Token(Token.END, self.line_i, i))
        n = self.indent_stack.pop()
        self.indent = self.indent[:n]
      if len(indent_s) != self.indent_stack[-1]:
        print ('ET002 {0}@{1}: Line indentation doesn\'t match to any ' +
               'of lines above').format(self.line_i + 1, i + 1)
        return
    self.indent = indent_s
    if self.indent_stack[-1] != len(indent_s):
      self.indent_stack.append(len(indent_s))
    self.line_i += 1
    return (s, self.line_i - 1, i)

  def getCommaLine(self):
    pass

  def getSlashLine(self):
    pass

def tokenizeLine(line, i, tokens):
  while i < len(line):
    if line[i].isspace():
      i += 1
      continue
    if line[i] == "'":
      pass
    if line[i : i + 2] == "r'" or line[i : i + 2] == "r'":
      pass
    if line[i] in 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' and line[i, i + 2] not in ["r'", "R'"]:
      # type
      pass
    elif line[i] in 'abcdefghijklmnopqrstuvwxyz':
      # id
      pass
    elif line[i] in '0123456789':
      # number integer (dec/hex/oct/bin), float
      pass
    elif line[i] == '(':
      tokens.append(Token(Token.LPAREN, line, i))
    elif line[i] == ')':
      tokens.append(Token(Token.RPAREN, line, i))

# src: List(String)
def compile(src):
  src = map(string.rstrip, lines)
  for i in range(len(src)):
    print '{0}: {1}'.format(i + 1, src[i])
  indent = ''
  indent_stack = [0]
  # tokens: List(Token)
  tokens = []
  line_i = 0
  MODE_NORMAL = 0
  MODE_CARRY_SLASH = 1
  MODE_CARRY_COMMA = 2
  mode = MODE_NORMAL
  carry_first = -1
  carry_n = 0
  while line < len(src):
    s = src[line_i]
    i = getLeftSpaceCount(s)
    if i == len(s) or s[i] == '#':
      line_i += 1
      continue

    indent_s = s[:i]
    if not suffixEq(indent, indent_s):
      print ('ET001 {0}@{1}: Line indentation space chars don\'t match ' +
             'to the prevous line').format(line + 1, i + 1)
      return
    if mode == MODE_NORMAL:
      if len(indent_s) > len(indent):
        tokens.append(Token(Token.BEGIN, line_i, i))
      else:
        while len(indent_s) < indent_stack[-1]:
          tokens.append(Token(Token.END, line_i, i))
          n = indent_stack.pop()
          indent = indent[:n]
        if len(indent_s) != indent_stack[-1]:
          print ('ET002 {0}@{1}: Line indentation doesn\'t match to any ' +
                 'of lines above').format(line_i + 1, i + 1)
          return
      indent = indent_s
      if indent_stack[-1] != len(indent_s):
        indent_stack.append(len(indent_s))
    else:
      common.fatal('{0}@{1}: Carrying is not implemented'.format(line_i + 1, 0))

    new_mode = tokenizeLine(line, i, tokens)
    if mode != new_mode:
      if mode == MODE_NORMAL:
        carry_first = line_i
        carry_n = 0
      elif new_mode == MODE_NORMAL:
        carry_first = -1
        carry_n = 0
      else:
        common.fatal('{0}@{1}: Invalid mode pair'.format(line_i + 1, 0))
    mode = new_mode
  if mode != MODE_NORMAL:
    print 'ET003 {0}@{1}: Multiline is not ended properly'.format(carry_first + 1, 0)
    return
  while indent_stack[-1] != 0:
    n = indent_stack.pop()
    tokens.append(Token(Token.END, line, 0))

  print map(str, tokens)
  return tokens

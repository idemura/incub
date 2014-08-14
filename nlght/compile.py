import common
import string

class Token:
  EOF = 0
  BEGIN = 10
  END = 11
  EOL = 12
  LPAREN = 20
  RPAREN = 21
  COMMA = 22

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

def error(abs_location, message):
  j = message.find(':')
  if j >= 0:
    m = message[:j] + '{0}:{1}@{2}'.format(*abs_location) + message[j:]
  else:
    m = message
  print m
  exit(-1)

class Liner:
  def __init__(self, file_name, lines, tokens):
    self.tokens = tokens
    self.file_name = file_name
    self.lines = map(string.rstrip, lines)
    self.line_i = -1
    self.indent_stack = [0]
    self.indent = ''
    self.wrap = False

  # # Count of indentation levels.
  # def getIndentCount(self):
  #   # First element is 0.
  #   return len(self.indent_stack) - 1

  def getIndent(self):
    return self.indent_stack[-1]

  def pushIndent(self, indent):
    if len(indent) < len(self.indent):
      error('R001: Indent check')
    if len(indent) > len(self.indent):
      self.indent = indent
      self.indent_stack.append(len(indent))

  def popIndent(self):
    n = self.indent_stack.pop()
    self.indent = self.indent[:n]
    return n

  def popIndentUntil(self, n):
    while self.indent_stack[-1] > n:
      j = self.popIndent()
      self.tokens.append(Token(Token.END, self.line_i, j))
    if self.indent_stack[-1] != n:
      error(self.getAbsLocation(0),
            'L002: Line indentation width mismatch')

  # def clearIndents(self):
  #   self.indent_stack = [0]
  #   self.indent = ''

  # Current line or None if EOF.
  def getCurrentLine(self):
    if self.line_i < len(self.lines):
      return self.lines[self.line_i]

  # Translates column position to (file, line, column).
  def getAbsLocation(self, col):
    return (self.file_name, self.line_i + 1, col + 1)

  # (line String | None, line_num I32).
  def getNonEmptyLine(self):
    if self.line_i == len(self.lines):
      return (None, self.line_i)
    self.line_i += 1
    while self.line_i < len(self.lines):
      s = self.lines[self.line_i]
      i = getLeftSpaceCount(s)
      if not (i == len(s) or s[i] == '#'):
        indent_s = s[:i]
        if not suffixEq(self.indent, indent_s):
          error(self.getAbsLocation(i),
                'L001: Indentation spaces mismatch')
        return (s, self.line_i)
      self.line_i += 1
    return (None, self.line_i)

  def getLine(self):
    if self.wrap:
      self.popIndent()
      self.wrap = False

    s, line_i = self.getNonEmptyLine()
    if s is None:
      # EOF
      self.popIndentUntil(0)
      return (s, line_i)

    i = getLeftSpaceCount(s)
    indent_s = s[:i]
    # if not suffixEq(self.indent, indent_s):
    #   print ('ET001 {0}@{1}: Line indentation space chars don\'t match ' +
    #          'to the prevous line').format(self.line_i + 1, i + 1)
    #   return
    # if len(indent_s) == len(self.indent):
    #   tokens.append(Token(Token.EOL, self.line_i, i))
    # elif len(indent_s) > len(self.indent):
    #   tokens.append(Token(Token.BEGIN, self.line_i, i))
    if len(indent_s) > len(self.indent):
      self.tokens.append(Token(Token.BEGIN, line_i, i))
    else:
      self.popIndentUntil(len(indent_s))
    self.pushIndent(indent_s)
    return (s, line_i)

  # (line String | None, line_num I32)
  def getWrappedLine(self):
    s, line_i = self.getNonEmptyLine()
    if s is None:
      return (s, line_i)
    i = getLeftSpaceCount(s)
    if self.wrap:
      if i != self.indent_stack[-1]:
        error(self.getAbsLocation(i),
              'L003: Wrapped line indentation width mismatch')
        return (None, line_i)
    else:
      if i <= self.indent_stack[-1]:
        error(self.getAbsLocation(i),
              'L004: Wrapped line indentation should be greater than start line')
        return (None, line_i)
      self.wrap = True
    return (s, line_i)

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
  tokens = []
  liner = Liner('<file>', src, tokens)
  s, line_i = liner.getLine()
  while s is not None:

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

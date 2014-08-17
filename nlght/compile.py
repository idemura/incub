import common
import string

class Token:
  EOF = 0
  BEGIN = 1
  END = 2
  EOL = 3
  # ${Tokens}
  IF = 10
  FN = 11
  FOR = 12
  WHILE = 13
  CLASS = 14
  VAR = 15
  CONST = 16
  RECORD = 17
  OBJECT = 18
  CASE = 19
  OF = 20
  ELSE = 21
  RETURN = 22
  TEMPLATE = 23
  TYPE = 24
  ENUM = 25
  VIRTUAL = 26
  PUBLIC = 27
  IMPORT = 28
  EXTERN = 29
  LPAREN = 100
  RPAREN = 101
  COMMA = 102
  LBRACKET = 103
  RBRACKET = 104
  COLON = 105
  NOT = 106
  TILDE = 107
  MOD = 108
  DIV = 109
  XOR = 110
  AND = 111
  OR = 112
  PLUS = 113
  MINUS = 114
  STAR = 115
  ASSIGN = 116
  GT = 117
  LT = 118
  # $END.

  keywords = {
    # ${Keywords}
    'case' : CASE,
    'class' : CLASS,
    'const' : CONST,
    'else' : ELSE,
    'enum' : ENUM,
    'extern' : EXTERN,
    'fn' : FN,
    'for' : FOR,
    'if' : IF,
    'import' : IMPORT,
    'object' : OBJECT,
    'of' : OF,
    'public' : PUBLIC,
    'record' : RECORD,
    'return' : RETURN,
    'template' : TEMPLATE,
    'type' : TYPE,
    'var' : VAR,
    'virtual' : VIRTUAL,
    'while' : WHILE,
    # $END.
  }

  symops = {
    # ${SymOps}
    '(' : LPAREN,
    ')' : RPAREN,
    ',' : COMMA,
    '[' : LBRACKET,
    ']' : RBRACKET,
    ':' : COLON,
    '!' : NOT,
    '~' : TILDE,
    '%' : MOD,
    '/' : DIV,
    '^' : XOR,
    '&' : AND,
    '|' : OR,
    '+' : PLUS,
    '-' : MINUS,
    '*' : STAR,
    '=' : ASSIGN,
    '>' : GT,
    '<' : LT,
    # $END.
  }

  def __init__(self, type, line, col, value = None):
    self.type = type
    self.line = line
    self.col = col
    self.value = value

  def __str__(self):
    s = 'Token type={type} line={line} col={col}'.format(**self)
    if self.value is not None:
      s += ' value={value}'.format(**self)
    return s

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

  def getIndent(self):
    return self.indent_stack[-1]

  # Pushes indent on the stack. If indent width the same as stack's top, does
  # nothing.
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
    return n  # Consistency with popIndent.

  # # Current line or None if EOF.
  # def getCurrentLine(self):
  #   if self.line_i < len(self.lines):
  #     return self.lines[self.line_i]

  # Translates column position to (file, line, column) for user message.
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
      # Wrapped line stores indentation on the stack.
      self.popIndent()
      self.wrap = False

    s, line_i = self.getNonEmptyLine()
    if s is None:
      # EOF. Emit END tokens for indents left in the stack.
      self.popIndentUntil(0)
      return (None, line_i)

    i = getLeftSpaceCount(s)
    indent_s = s[:i]
    if len(indent_s) > len(self.indent):
      self.tokens.append(Token(Token.BEGIN, line_i, i))
    else:
      # If indent is the same, does nothing.
      self.popIndentUntil(len(indent_s))
    self.pushIndent(indent_s)
    return (s, line_i)

  # (line String | None, line_num I32)
  def getWrappedLine(self):
    s, line_i = self.getNonEmptyLine()
    if s is None:
      # Wrap is obviously not finished, but tokenizer will handle this.
      return (None, line_i)
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

def isIdFirst(ch):
  return ch in 'abcdefghijklmnopqrstuvwxyz'

def isTypeIdFirst(ch):
  return ch in 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

def takeId(line, i):
  j = i
  while line[j] in 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_':
    j += 1
  return (j, line[i : j])

def tokenizeLine(liner, tokens):
  (line, line_i) = liner.getLine()
  if line is None:
    return False
  i = 0
  while i < len(line):
    if line[i].isspace():
      i += 1
      continue

    if isTypeIdFirst(line[i]):
      first = i
      while True:
        # Bangs are treated as part of type id
      (i, id_s) = takeId(line, i)
      tokens.append(Token(Token.TYPE_ID, line_i, first, id_s))
    elif isIdFirst(line[i]):
      first = i
      (i, id_s) = takeId(line, i)
      if id_s in Token.keyword_map:
        tokens.append(Token(Token.keyword_map[id_s], line_i, first, id_s))
      else:
        tokens.append(Token(Token.ID, line_i, first, id_s))
    elif line.startwith('('


  return True

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
  while tokenizeLine(liner, tokens):
    pass

  print map(str, tokens)
  return tokens

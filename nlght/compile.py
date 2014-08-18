import common
import string

EOF = 0
BEGIN = 1
END = 2
EOL = 3
STRING_LITERAL = 4
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

KEYWORDS = {
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

SYMOPS = {
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

class Token:
  def __init__(self, type, loc, value = None):
    self.type = type
    self.loc = loc
    self.value = value

  def __str__(self):
    s = 'Token {type} at {loc}'.format(**self.__dict__)
    if self.value is not None:
      s += ' value={value}'.format(**self.__dict__)
    return s

def countLeftSpaces(s):
  i = 0
  while i < len(s) and s[i].isspace():
    i += 1
  return i

def suffixEq(s1, s2):
  ml = min(len(s1), len(s2))
  return s1[:ml] == s2[:ml]

def error(loc, message):
  m = message
  if loc is not None:
    j = message.find(':')
    if j >= 0:
      m = message[:j] + '{0}:{1}@{2}'.format(*loc) + message[j:]
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
      error(None, 'R001: Indent check')
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
      self.tokens.append(Token(END, self.getAbsLocation(j)))
    if self.indent_stack[-1] != n:
      error(self.getAbsLocation(0),
            'L002: Line indentation width mismatch')
    return n  # Consistency with popIndent.

  # Translates column position to (file, line, column) for user message.
  def getAbsLocation(self, col):
    return (self.file_name, self.line_i + 1, col + 1)

  def getLineIndex(self):
    return self.line_i

  # (line String | None, line_num I32).
  def getNonEmptyLine(self):
    if self.line_i == len(self.lines):
      return None

    self.line_i += 1
    while self.line_i < len(self.lines):
      s = self.lines[self.line_i]
      i = countLeftSpaces(s)
      if not (i == len(s) or s[i] == '#'):
        indent_s = s[:i]
        if not suffixEq(self.indent, indent_s):
          error(self.getAbsLocation(i),
                'L001: Indentation spaces mismatch')
        return s
      self.line_i += 1
    return None

  def getLine(self):
    if self.wrap:
      # Wrapped line stores indentation on the stack.
      self.popIndent()
      self.wrap = False

    s = self.getNonEmptyLine()
    if s is None:
      # EOF. Emit END tokens for indents left in the stack.
      self.popIndentUntil(0)
      return None

    i = countLeftSpaces(s)
    indent_s = s[:i]
    if i > len(self.indent):
      self.tokens.append(Token(BEGIN, self.getAbsLocation(i)))
    elif i < len(self.indent):
      self.popIndentUntil(i)
    else:
      self.tokens.append(Token(EOL, self.getAbsLocation(i)))
    self.pushIndent(indent_s)
    return s

  def getWrappedLine(self):
    s = self.getNonEmptyLine()
    if s is None:
      # Wrap is obviously not finished, but tokenizer will handle this.
      return None
    i = countLeftSpaces(s)
    if self.wrap:
      if i != self.indent_stack[-1]:
        error(self.getAbsLocation(i),
              'L003: Wrapped line indentation width mismatch')
    else:
      if i <= self.indent_stack[-1]:
        error(self.getAbsLocation(i),
              'L004: Wrapped line indentation should be greater than start line')
      self.wrap = True
    return s

def isIdFirst(ch):
  return ch in 'abcdefghijklmnopqrstuvwxyz'

def isTypeIdFirst(ch):
  return ch in 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

def takeId(line, i):
  j = i
  while line[j] in 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_':
    j += 1
  return (j, line[i : j])

char_escapes = {
  'n' : '\n', 'r': '\r', 't': '\t', 's': ' ', 'a': '\a', 'b': '\b', '\\': '\\'
}

def tokenizeLine(liner, tokens):
  line = liner.getLine()
  if line is None:
    return False

  i = 0
  while i < len(line):
    if line[i].isspace():
      i += 1
      continue

    if isTypeIdFirst(line[i]):
      first = i
      # TODO: Bangs are treated as part of type id in tokenizer.
      i, id_s = takeId(line, i)
      tokens.append(Token(TYPE_ID, liner.getAbsLocation(first), id_s))
    elif isIdFirst(line[i]):
      first = i
      i, id_s = takeId(line, i)
      if id_s in KEYWORDS:
        tokens.append(Token(KEYWORDS[id_s], liner.getAbsLocation(first), id_s))
      else:
        tokens.append(Token(ID, liner.getAbsLocation(first), id_s))
    elif line[i: i + 2] in SYMOPS:
      tokens.append(Token(SYMOPS[line[i: i + 2]], liner.getAbsLocation(first)))
      i += 2
    elif line[i: i + 1] in SYMOPS:
      tokens.append(Token(SYMOPS[line[i: i + 1]], liner.getAbsLocation(first)))
      i += 1
      # TODO: Check if `, `( or `[ ends the line.
    elif line[i] == '"':
      literal_first = i
      literal = ''
      while i < len(line) and line[j] != '"':
        if line[i] == '\\':
          i += 1
          if i < len(line):
            # Escaped character.
            if line[i] in char_escapes:
              literal.append(char_escapes[line[i]])
            else:
              error(liner.getAbsLocation(i),
                    'T002: Unknown escape char {0}'.format(line[j]))
          else:
            # Wrap the line.
            line = liner.getWrappedLine()
            i = countLeftSpaces(line)
            if line[i] != '\\':
              error(liner.getAbsLocation(i),
                    'T001: \\ wrappend line should begin with \\')
        else:
          # TODO: if space other than space, error, or bad utf8.
          literal.append(line[j])
      if i == len(line):
        error(liner.getAbsLocation(i),
              'T003: String literal isn\'t terminated')
      tokens.append(Token(STRING_LITERAL, liner.getAbsLocation(literal_first),
                          literal))
  return True

# src: List(String)
def compile(file_name, src):
  tokens = []
  liner = Liner(file_name, src, tokens)
  while tokenizeLine(liner, tokens):
    pass

  print map(str, tokens)
  return tokens

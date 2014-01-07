'use strict';

function print() {
  console.log.apply(null, arguments);
}

function calc(expr) {
  function tokenize(expr) {
    var i = 0;
    function isEof() {
      return i == expr.length;
    }
    function isDigit() {
      return '0123456789'.indexOf(expr[i]) >= 0;
    }
    function token() {
      while (!isEof() && expr[i] == ' ') {
        i++;
      }
      var pos = i;
      if (isEof()) {
        return {
          tokType: 'eof',
          pos: pos
        };
      }
      var symbols = '+-*/%^()';
      var k = symbols.indexOf(expr[i]);
      if (k >= 0) {
        i++;
        return {
          tokType: symbols[k],
          pos: pos
        };
      }
      if (isDigit() >= 0) {
        var i0 = i;
        do {
          i++;
        } while (!isEof() && isDigit());
        return {
          tokType: 'number',
          pos: pos,
          value: parseInt(expr.substring(i0, i), 10)
        };
      }
      return {
        pos: pos,
        error: 'unknown symbol'
      };
    }

    var ts = [];
    for (var t = token(); t.tokType; t = token()) {
      ts.push(t);
      if (t.tokType === 'eof')
        break;
    }
    if (t.tokType) {
      return { tokens: ts };
    } else {
      return t;
    }
  }

  function calcImpl(tokens) {
    if (!tokens.length) {
      return {
        error: 'empty expression',
        pos: 1
      };
    }

    var numbers = [], ops = [];
    var tok_index = 0;

    function tok() {
      return tokens[tok_index++];
    }
    function undoToken() {
      tok_index--;
    }
    function calcStack() {
      while (ops.length && ops[ops.length-1].tokType !== '(') {
        var b = numbers.pop().value;
        var a = numbers.pop().value;
        var r;
        switch (ops.pop().tokType) {
          case '+':
            r = a + b;
            break;
          case '-':
            r = a - b;
            break;
          case '*':
            r = a * b;
            break;
          case '/':
            r = a / b;
            break;
          case '%':
            r = a % b;
            break;
          case '^':
            r = Math.pow(a, b);
            break;
        }
        numbers.push({tokType: 'number', pos: -1, value: r});
      }
    }
    function prio(op) {
      switch (op) {
        case '+': case '-':
          return 10;
        case '*': case '/': case '%':
          return 20;
        case '^':
          return 30;
      }
    }
    function left(op) {
      return '+-*/'.indexOf(op) >= 0;
    }
    function opEnd() {
      var t = tok();
      switch (t.tokType) {
        case '+': case '-':
        case '*': case '/': case '%':
        case '^':
          if (ops.length) {
            var p0 = prio(ops[ops.length-1].tokType);
            var pt = prio(t.tokType);
            if (p0 > pt || (p0 == pt && left(pt))) {
              calcStack();
            }
          }
          ops.push(t);
          break;
        case ')':
        case 'eof':
          undoToken();
          break;
        default:
          return {
            error: 'expected operator',
            pos: t.pos
          };
      }
    }

    for (var t = tok(); t.tokType && t.tokType !== 'eof'; t = tok()) {
      switch (t.tokType) {
        case 'number':
          numbers.push(t);
          var e = opEnd();
          if (e)
            return e;
          break;
        case '(':
          ops.push(t);
          break;
        case ')':
          calcStack();
          if (!ops.length || ops[ops.length-1].tokType !== '(') {
            return {
              error: 'unmatched \'(\'',
              pos: t.pos
            };
          } else {
            ops.pop();
          }
          var err = opEnd();
          if (err)
            return err;
          break;
        default:
          return {
            error: 'unexpected token',
            pos: t.pos
          };
      }
    }
    calcStack();
    if (ops.length) {
      var suberr = ops[ops.length-1].tokType === '('? ' (unmatched \'(\')': '';
      return {
        error: 'operations stack is not empty' + suberr,
        pos: -1
      };
    } else {
      return {
        value: numbers.pop().value
      };
    }
  }

  var ts = tokenize(expr);
  if (ts.tokens) {
    return calcImpl(ts.tokens);
  } else {
    return ts;
  }
}

(function() {
  function charMarker(n) {
    for (var s = ''; n > 0; n--, s += ' ') {
    }
    return s + '^';
  }
  function check(expr, expected) {
    var res = calc(expr);
    if (res.value === undefined) {
      print(expr);
      print(charMarker(res.pos));
      print('Error @' + (res.pos + 1) + ': ' + res.error);
    } else {
      print(expr + ' = ' + res.value);
      if (res.value != expected) {
        print('EXPECTED: ' + expected);
      }
    }
  }

  check("9", 9);
  check("2 + 3", 5);
  check("2 + 3 - 1", 4);
  check("1 + 3 * 2", 7);
  check("3 * 2 - 1", 5);
  check("4 * 3 / 2", 6);
  check("2 * (1 + 3)", 8);
  check("(1 + 3) * 2", 8);
  check("2 + (1 + 3)", 6);
  check("(1 + 3) + 2", 6);
  check("(1 + 2) * (5 - 3)", 6);
  check("(1 + 2) ^ 2", 9);
  check("2 ^ 3 ^ 2", 512);
}());

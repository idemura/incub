function log() {
  console.log.apply(null, arguments);
}

function range(a, i, j) {
  a.slice(Math.max(0, i), Math.min(j, a.length));
}

function gcd(a, b) {
  var t;
  if (a < b) {
    t = a; a = b; b = t;
  }
  if (b === 0) {
    return;
  }
  while (b) {
    t = a % b;
    a = b;
    b = t;
  }
  return a;
}

// (function() {
//   log('GCD', gcd(12, 8));
//   log('GCD', gcd(15, 14));
// }());

var gray = {
  encode: function (n) {
    return n ^ (n >>> 1);
  },

  decode: function (g) {
    for (var n = 0; g !== 0; g >>>= 1) {
      n ^= g;
    }
    return n;
  }
};

function binStr(n, width) {
  var s = n.toString(2);
  while (s.length < width) {
    s = '0' + s;
  }
  return s;
}

// (function() {
//   var WIDTH = 3;
//   for (var i = 0; i < 8; i++) {
//    var g = gray.encode(i);
//    log(binStr(i, WIDTH), binStr(g, WIDTH), 'decode:',
//        binStr(gray.decode(g), WIDTH));
//   }
// }());

function primes(n) {
  var a = new Array(n + 1);
  var p = 2, pMax = Math.floor(Math.sqrt(n));
  for (; p <= pMax; p++) {
    if (!a[p]) {
      for (var i = p * p; i <= n; i += p) {
        a[i] = true;
      }
    }
  }
  var out = [];
  for (p = 2; p <= n; p++) {
    if (!a[p])
      out.push(p);
  }
  return out;
}

function isPrime(a) {
  if (a % 2 === 0 || a % 3 === 0) {
    return false;
  }
  var d = 5, s = 2;
  while (d * d <= a) {
    if (a % d === 0)
      return false;
    d += s;
    s = 6 - s;
  }
  return true;
}

// (function() {
//   log(isPrime(12));
//   log(isPrime(23));
//   log(isPrime(982451653));  // This is prime.
//   log(primes(16));
// }());

function binarySearch(a, x) {
  var i = 0, j = a.length;
  while (i < j) {
    var m = Math.floor((i + j) / 2);
    // For upper bound x <= a[m].
    if (x <= a[m]) {
      j = m;
    } else {
      i = m + 1;
    }
  }
  return i;
}

// (function() {
//   function check(x) {
//     var i = binarySearch(a, x);
//     log('x=' + x + ' @' + i + ' ' + a.slice(i - 1, i + 2));
//   }
//
//   var a = [1, 4, 6, 8, 8, 10, 11];
//   check(0);
//   check(5);
//   check(6);
//   check(8);
//   check(10);
// }());

// `a` is [[x]]
function project(a) {
  return a.map(function (x) { return x[0]; });
}

function isIntersect(s, segms) {
  var i = binarySearch(project(segms), s[0]);
  return (i > 0 && s[0] < segms[i - 1][1]) ||
         (i < segms.length && s[1] > segms[i][0]);
}

// (function() {
//   function check(s, expected) {
//     var intersect = isIntersect(s, segms);
//     log('[' + s[0] + ', ' + s[1] + '] is ' + intersect);
//     if (intersect != expected) {
//       log('EXPECTED: ' + expected);
//     }
//   }
//
//   var segms = [[3, 5], [10, 13], [16, 18]];
//   check([1, 2], false);
//   check([1, 3], false);
//   check([1, 4], true);
//   check([4, 6], true);
//   check([6, 8], false);
//   check([4, 12], true);
//   check([9, 14], true);
//   check([15, 18], true);
//   check([17, 19], true);
//   check([15, 19], true);
//   check([19, 20], false);
// }());

// (~k + 1) is the same as -k and value is least bin that is != 0.
function BIT(n) {
  var a = [];
  a[n] = undefined;
  for (var i = 0; i <= n; i++) {
    a[i] = 0;
  }
  this.a = a;
  this.length = n;
}

BIT.prototype.update = function(i, d) {
  while (i < this.a.length) {
    this.a[i] += d;
    i += i & -i;
  }
};

BIT.prototype.sum = function(i) {
  var sum = 0;
  while (i) {
    sum += this.a[i];
    i -= i & -i;
  }
  return sum;
};

// Although better method exists, this is OK. Handles border case.
BIT.prototype.get = function(i) {
  var si = this.sum(i);
  if (i > 1) {
    si -= this.sum(i - 1);
  }
  return si;
};

BIT.prototype.toArray = function() {
  var a = [];
  for (var i = 1, l = this.a.length; i < l; i++) {
    a[i - 1] = this.sum(i);
  }
  return a;
};

// (function() {
//   var bit = new BIT(10);
//   bit.update(4, 3);
//   log(bit.toArray());
//   bit.update(5, 1);
//   log(bit.toArray());
//   bit.update(7, 1);
//   log(bit.toArray());
//   bit.update(1, 2);
//   log(bit.toArray());
//   log(bit.get(1));
//   log(bit.get(2));
//   log(bit.get(4));
//   log(bit.get(5));
//   log(bit.get(6));
//   log(bit.get(7));
// }());

function calc(expr) {
  function tokenize(expr) {
    var i = 0;
    function isEof() {
      return i == expr.length;
    }
    function isDigit() {
      var cc = expr.charCodeAt(i);
      return 47 < cc && cc < 58;
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
        }
      }
      var symbols = '+-*/()';
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
        }
        numbers.push({tokType: 'number', pos: -1, value: r});
      }
    }
    function prio(op) {
      switch (op) {
        case '+': case '-':
          return 10;
        case '*': case '/':
          return 20;
      }
    }
    function opEnd() {
      var t = tok();
      switch (t.tokType) {
        case '+':
        case '-':
        case '*':
        case '/':
          if (!ops.length ||
              prio(ops[ops.length-1].tokType) >= prio(t.tokType)) {
            // Left-associative handler with (>=).
            calcStack();
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

(function () {
  function charMarker(n) {
    for (var s = ''; n > 0; n--, s += ' ') {
    }
    return s + '^';
  }
  function check(expr, expected) {
    var res = calc(expr);
    if (res.value === undefined) {
      log(expr);
      log(charMarker(res.pos));
      log('Error @' + (res.pos + 1) + ': ' + res.error);
    } else {
      log(expr + ' = ' + res.value);
      if (res.value != expected) {
        log('EXPECTED: ' + expected);
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
}());

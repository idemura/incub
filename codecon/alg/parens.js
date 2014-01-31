'use strict';

function parens(n) {
  var parMem = [[''], ['()']];
  var i, j, i1, j1;

  function combine(p1, p2, r) {
    var i1, n1 = p1.length, i2, n2 = p2.length;
    for (i1 = 0; i1 < n1; i1++) {
      for (i2 = 0; i2 < n2; i2++) {
        r.push('(' + p1[i1] + ')' + p2[i2]);
      }
    }
  }

  for (i = 2; i <= n; i++) {
    parMem[i] = [];
    for (j = 0; j < i; j++) {
      combine(parMem[j], parMem[i - j - 1], parMem[i]);
    }
  }
  return parMem[n];
}

var par = parens(5);
console.log(par);
console.log('count: ' + par.length);
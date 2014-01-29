'use strict';

function parens(n) {
  var parMem = [[''], ['()']];
  var i, j;

  function combine(p1, p2, r) {
    var i1, n1 = p1.length, i2, n2 = p2.length;
    for (i1 = 0; i1 < n1; i1++) {
      for (i2 = 0; i2 < n2; i2++) {
        // To avoid duplications, only surround the first group.
        r.push('(' + p1[i1] + ')' + p2[i2]);
      }
    }
  }

  // Size 0 and 1 generated, go 2..n.
  for (i = 2; i <= n; i++) {
    parMem[i] = [];
    // Take i-1 parens and split them to the left and right group and combine.
    for (j = 0; j < i; j++) {
      combine(parMem[j], parMem[i - j - 1], parMem[i]);
    }
  }
  return parMem[n];
}

var par = parens(5);
console.log(par);
console.log('count: ' + par.length);

'use strict';

function print() {
  console.log.apply(null, arguments);
}

function unique(as) {
  var us = [];
  as.sort();  // OK to sort lexicographically.
  for (var i = 1; i < as.length; i++) {
    if (as[i - 1] != as[i]) {
      us.push(as[i]);
    }
  }
  return us;
}

var s = 'asddsaasdd';
// var s = 'facargjjg';
// var s = 'abba';

var sLen = s.length;
var pals = [[], []], res = [];
for (var i = 0; i < sLen; i++) {
  pals[1][i] = pals[0][i] = i;
  res.push(s[i]);
}
for (var l = 0; l < sLen; l++) {
  var p = pals[l % 2], updated = [];
  for (var i = 0; i < p.length; i++) {
    var j = p[i];
    if (0 < j && j + l < sLen && s[j - 1] == s[j + l]) {
      updated.push(j - 1);
      res.push(s.slice(j - 1, j + l + 1));
    }
    pals[l % 2] = updated;
  }
}
print(s + ':');
print(unique(res));

// var pals = [[], []], res = [];
// for (var i = 0; i < sLen; i++) {
//   pals[1][i] = pals[0][i] = true;
//   res.push(s[i]);
// }
// for (var l = 1; l < sLen; l++) {
//   var p = pals[1 - l % 2];
//   for (var i = 0; i + l < sLen; i++) {
//     p[i] = p[i + 1] && s[i] == s[i + l];
//     if (p[i]) {
//       res.push(s.slice(i, i + l + 1));
//     }
//   }
// }
// print(s + ':');
// print(unique(res));

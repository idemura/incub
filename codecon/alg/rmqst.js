'use strict';

function print() {
  console.log.apply(null, arguments);
}

// RMQ with spare table.
function RMQ_ST(as) {
  var st = [];
  st[0] = as.slice();  // Shallow copy.
  for (var l = 1, i = 1; l < as.length; l *= 2, i++) {
    st[i] = [];
    for (var j = 0; j + l < st[i - 1].length; j++) {
      st[i][j] = Math.min(st[i - 1][j], st[i - 1][j + l]);
    }
  }
  this.st = st;
}

RMQ_ST.prototype.print = function() {
  var st = this.st;
  for (var i = 0; i < st.length; i++) {
    var len = 1 << i;
    print('Length ' + len + ':');
    var v = st[i];
    for (var j = 0; j < v.length; j++) {
      print('  Min in ' + st[0].slice(j, j + len) + ' is ' + v[j]);
    }
  }
};

RMQ_ST.prototype.RMQ = function(i, j) {
  var st = this.st;
  for (var d = 0; 2 << d < j - i; d++) {
  }
  return Math.min(st[d][i], st[d][j - (1<<d)]);
};

(function() {
  function check(as, rs) {
    var st = new RMQ_ST(as);
    // st.print();
    for (var i = 0; i < rs.length; i++) {
      var t = rs[i];
      var m = st.RMQ(t[0], t[1]);
      // Check min computed straightforward.
      var mc = 9999;
      for (var j = t[0]; j < t[1]; j++) {
        if (as[j] < mc) {
          mc = as[j];
        }
      }
      if (m != mc) {
        print('In range ' + as.slice(t[0], t[1]) + ' min is ' + mc +
              ', but ST ' + m);
      }
    }
  }

  check([2, 1, 4, 3, 6, 5], [[0, 3], [0, 1], [2, 3], [0, 6], [3, 6]]);
  check([2, 1, 4, 3], [[0, 4], [0, 1], [1, 4], [1, 3]]);
}());

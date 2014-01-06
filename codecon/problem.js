'use strict';

function print() {
  console.log.apply(null, arguments);
}

// RMQ with spare table.
function RMQ_ST(as) {
  var st = [];
  st[0] = as.slice();  // Shallow copy.
  for (var l = 1, i = 0; l <= as.length; l *= 2, i++) {
    for (var j = 0; j + l < as.length; j++) {
      st[i + 1][j] = Math.max(st[i][j], st[i][j + l]);
    }
  }
  this.st = st;
}

RMQ_ST.prototype.print = function() {
  for (var i = 0; i < this.st.length; i++) {
    var len = 1 << i;
    print('Length ' + len + ':');
    var v = this.st[i];
    for (var j = 0; j < v.length; j++) {
      print('  Max in ' + this.st[0].slice(j, j + len) + ' is ' + v[j]);
    }
  }
};

RMQ_ST.prototype.RMQ = function(i, j) {
  var d = 0;
  for (var d = 0; (1<<d) < (i - j); d++) {
  }
  return Math.max(this.st[d][i], this.st[d][j - (1<<d)]);
};

(function() {
  function check(as, rs) {
    var st = new RMQ_ST(as);
    st.print();
    for (var i = 0; i < rs.length; i++) {
      var t = rs[i];
      var m = st.RMQ(t[0], t[1]);
      // Check with min.
      var mc = 9999;
      for (var j = t[0]; j < t[1]; j++) {
        if (as[j] < mc) {
          mc = as[j];
        }
      }
      if (m != mc) {
        print('In range ' + t[0] + '-' + t[1] + ' min is ' + mc +
              ', but ST ' + m);
      }
    }
  }
  check([2, 1, 4, 3, 6, 5], [[0, 3]]);
}());

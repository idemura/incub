'use strict';

var lib = require('./lib');

function Scanner(text) {
  this.splits = text.split(/\s+/).filter(function(s) { return s.length > 0; });
  this.p = 0;
}

Scanner.prototype.getInt = function() {
  if (this.p < this.splits.length) {
    return parseInt(this.splits[this.p++], 10);
  }
}

Scanner.prototype.getStr = function() {
  if (this.p < this.splits.length) {
    return this.splits[this.p++];
  }
}

Scanner.prototype.eof = function() {
  return this.p == this.splits.length;
}

function input(file, callback) {
  var fs = require('fs');
  fs.readFile(file, 'utf8', function(err, data) {
    if (err) {
      process.exit(1);
    }
    // Split on spaces, save that vector. have methods get int, string.
    callback(new Scanner(data));
  });
}

input(process.argv[2], function(sc) {
  function check(heap) {
    heap.check();
  }

  var h = new lib.Heap();
  h.build([5, 6, 3, 2, 4, 1]);
  check(h);
  h.insert(5.5);
  check(h);
  h.insert(0);
  check(h);
  console.log(h.remove());
});

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

// input(process.argv[2], function(sc) {
// });

var Heap = lib.Heap;

Heap.prototype.check = function() {
  var i, p, h = this.h;
  for (i = 1; i < h.length; i++) {
    p = (i - 1) >>> 1;
    if (!this.cmp(h[p], h[i])) {
      console.log('Parent ' + h[p] + ' is greater than ' + a[i]);
      console.log('  Heap ' + h);
    }
  }
}

var h = new Heap();
h.build([6, 4, 3, 1, 0, 5, 2, 8, 7]);
h.check();

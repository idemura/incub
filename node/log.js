'use strict';

var util = require('util');

(function(mmap) {
  function print() {
    util.log.apply(null, arguments);
  }

  function error() {
    util.error.apply(null, arguments);
  }

  mmap.error = error;
  mmap.print = print;
}(module.exports));

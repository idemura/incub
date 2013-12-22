'use strict';

(function(mmap) {
  function print() {
    console.log.apply(null, arguments);
  }

  function error() {
    console.error.apply(null, arguments);
  }

  mmap.error = error;
  mmap.print = print;
}(module.exports));

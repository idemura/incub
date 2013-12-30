'use strict';

var util = require('util');

function die(object) {
  error('FATAL ERROR. Process is exiting.');
  error(object);
  process.exit(1);
}

function error() {
  util.error.apply(null, arguments);
}

function print() {
  console.log.apply(null, arguments);
}

function trace() {
  console.log.apply(null, arguments);
}

exports.die = die;
exports.error = error;
exports.print = print;
exports.trace = trace;

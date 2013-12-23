'use strict';

var util = require('util');

function error() {
  util.error.apply(null, arguments);
}

function print() {
  util.log.apply(null, arguments);
}

function trace() {
  console.log.apply(null, arguments);
}

exports.error = error;
exports.print = print;
exports.trace = trace;

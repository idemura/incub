'use strict';

var util = require('util');

var fatalHandler;

function die(object) {
  error('FATAL ERROR. Process is exiting.');
  error(object);
  process.exit(1);
}

function error() {
  util.error.apply(null, arguments);
}

function fatal() {
  util.error.apply(null, arguments);
  if (fatalHandler) {
    fatalHandler();
  }
}

function onFatal(h) {
  fatalHandler = h;
}

function print() {
  console.log.apply(null, arguments);
}

function trace() {
  console.log.apply(null, arguments);
}

exports.die = die;
exports.error = error;
exports.fatal = fatal;
exports.print = print;
exports.onFatal = onFatal;
exports.trace = trace;

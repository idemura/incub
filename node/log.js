'use strict';

function die(object) {
  error('FATAL ERROR. Process is exiting.');
  error(object);
  process.exit(1);
}

function error() {
  console.error.apply(null, arguments);
}

function fatal() {
  console.error.apply(null, arguments);
  console.trace();
}

function print() {
  console.log.apply(null, arguments);
}

function debug() {
  console.log.apply(null, arguments);
}

exports.debug = debug;
exports.die = die;
exports.error = error;
exports.fatal = fatal;
exports.print = print;

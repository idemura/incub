'use strict';

var fs = require('fs');
var mustache = require('mustache');
var path = require('path');

function readTemplates(dir, callback) {
  var count = 0, tpl = {};
  function read(name) {
    fs.readFile(path.join(dir, name), 'utf8', function(err, data) {
      if (err) {
        callback(err, null);
      }
      tpl[path.basename(name, '.html')] = data;
      if (--count === 0) {
        callback(null, tpl);
      }
    });
  }

  fs.readdir(dir, function(err, files) {
    files = files.filter(function(name) {
      return path.extname(name) == '.html';
    });
    count = files.length;
    files.forEach(read);
  });
};

function render(tpl, view) {
  return mustache.render(tpl, view);
}

exports.readTemplates = readTemplates;
exports.render = render;

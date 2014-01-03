'use strict';

var fs = require('fs');
var mustache = require('mustache');
var path = require('path');

var templates = {};
function readTemplates(dir, callback) {
  var count = 0;
  function read(name) {
    fs.readFile(path.join(dir, name), 'utf8', function(err, data) {
      if (err) {
        callback(err);
      }
      templates[path.basename(name)] = data;
      if (--count === 0) {
        callback(null);
      }
    });
  }

  fs.readdir(dir, function(err, files) {
    files = files.filter(function(name) {
      var ext = path.extname(name);
      return ext === '.html' || ext === '.json';
    });
    count = files.length;
    files.forEach(read);
  });
};

function render(tpl, view) {
  var t = templates[tpl];
  if (t) {
    return mustache.render(t, view);
  } else {
    log.error('ERROR rendering ' + tpl);
    return;
  }
}

exports.readTemplates = readTemplates;
exports.render = render;

'use strict';

var fs = require('fs');
var lib = require('./lib');
var log = require('./log');

var cfg = {
  read: function(callback) {
    fs.readFile(lib.getCmdLineArg('--config', 'config.json'),
                {encoding: 'utf8'}, function(err, data) {
      if (err) {
        callback(err);
      } else {
        var cfgFileOpts = JSON.parse(data);
        for (var k in cfgFileOpts) {
          if (k !== 'read') {
            cfg[k] = cfgFileOpts[k];
          }
        }
        callback(null);
      }
    });
  }
};

module.exports = exports = cfg;

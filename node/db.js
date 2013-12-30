'use strict';

var lib = require('./lib');
var log = require('./log');
var pg = require('pg');

function DB(address) {
  this.address = address;
  return this;
}

DB.prototype.finish = function() {
  this.clientFinish();
};

DB.prototype.query = function(stmt, params, callback) {
  if (!params) {
    params = [];
  } else if (lib.typeOf(params) == 'function') {
    callback = params;
    params = [];
  }

  var stmtIndexParams = "";
  var k = 1;
  for (var i = 0, n = stmt.length; i < n; i++) {
    if (stmt[i] == '?') {
      stmtIndexParams += '$' + k++;
    } else {
      stmtIndexParams += stmt[i];
    }
  }
  stmt = stmtIndexParams;

  this.client.query(stmt, params, function(err, rows) {
    if (err) {
      log.error('PG error in: ' + stmt, err);
    }
    if (callback) {
      callback(err, rows);
    }
  });
}

DB.prototype.on = function(ev, callback) {
  this.client.on(ev, callback);
}

function connect(address, callback) {
  var self = new DB(address);
  pg.connect(address, function(err, client, finish) {
    if (err) {
      callback(err, null);
    }
    client.on('error', function(err) {
      log.error('DB error', err);
    });
    self.client = client;
    self.clientFinish = finish;
    callback(null, self);
  });
}

exports.connect = connect;

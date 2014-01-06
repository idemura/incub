'use strict';

var lib = require('./lib');
var log = require('./log');
var pg = require('pg');
var util = require('util');

function DB(address) {
  this.address = address;
}

DB.prototype.finish = function() {
  this.clientFinish();
  this.client.removeListener('error', this.dbErrorListener);
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

  this.client.query(stmt, params, function(err, dbres) {
    if (err) {
      log.error('DB error in: ' + stmt, err);
    }
    if (callback) {
      callback(err, dbres);
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
    self.dbErrorListener = function(err) {
      log.error('DB error', err);
    };
    client.on('error', self.dbErrorListener);
    self.client = client;
    self.clientFinish = finish;
    callback(null, self);
  });
}

function createTable(db, table) {
  var column_def = [];
  for (var k in table.columns) {
    column_def.push(k + ' ' + table.columns[k]);
  }
  var stmt = util.format('CREATE TABLE %s (%s);', table.name,
                         column_def.join(', '));
  db.query(stmt);

  if (table.indices) {
    for (var i = 0, n = table.indices.length; i < n; i++) {
      var c = table.indices[i];
      var name = c.name? c.name: table.name + '_by_' + c.column;
      var stmt = util.format('CREATE %s INDEX %s ON %s (%s);',
                             c.unique? 'UNIQUE': '', name, table.name,
                             c.column);
      db.query(stmt);
    }
  }
}

function createSchema(address, callback) {
  function defaultErrorHandler(err, dbres) {
    if (err) {
      return log.fatal(err);
    }
  }

  connect(address, function(err, db) {
    if (err) {
      return log.fatal(err);
    }
    createTable(db, {
      name: 'Groups',
      columns: {
        rowid: 'SERIAL UNIQUE',
        name: 'TEXT',
        privileges: 'TEXT'
      },
      indices: [
        // { column: 'rowid', unique: true },
        { column: 'name', unique: true }
      ]
    });
    (function() {
      var groups = [
        {name: 'admin', privileges: {admin: true}},
        {name: 'guest', privileges: {admin: false}},
        {name: 'user',  privileges: {admin: false}}];
      for (var i = 0; i < groups.length; i++) {
        var g = groups[i];
        g.privileges = JSON.stringify(g.privileges);
        var keys = lib.keys(g);
        db.query(lib.insertSql('Groups', keys), lib.values(g, keys),
                 defaultErrorHandler);
      }
    }());
    createTable(db, {
      name: 'Accounts',
      columns: {
        rowid: 'SERIAL UNIQUE',
        userid: 'TEXT',
        email: 'TEXT',
        gplus_id: 'TEXT',
        name: 'TEXT',
        given_name: 'TEXT',
        picture: 'TEXT',
        gender: 'TEXT',
        locale: 'TEXT',
        pgroup: 'INTEGER REFERENCES Groups(rowid)'
      },
      indices: [
        // { column: 'rowid', unique: true },
        { column: 'userid', unique: true },
        { column: 'email', unique: true },
        { column: 'gplus_id', unique: true },
        { column: 'name' }
      ]
    });
    createTable(db, {
      name: 'Sessions',
      columns: {
        rowid: 'SERIAL UNIQUE',
        session_id: 'TEXT',
        account_id: 'INTEGER REFERENCES Accounts(rowid)',
        create_time: 'BIGINT',
        access_time: 'BIGINT',
        meta: 'TEXT'
      },
      indices: [
        // { column: 'rowid', unique: true },
        { column: 'account_id' },
        // Should be unique, but collisions have tiny probability.
        { column: 'session_id' },
        { column: 'create_time' },
        { column: 'access_time' }
      ]
    });
    createTable(db, {
      name: 'Posts',
      columns: {
        rowid: 'SERIAL UNIQUE',
        account_id: 'INTEGER REFERENCES Accounts(rowid)',
        create_time: 'BIGINT',
        modify_time: 'BIGINT',
        link: 'TEXT',
        tags: 'TEXT',
        text: 'TEXT'
      },
      indices: [
        // { column: 'rowid', unique: true },
        { column: 'account_id' },
        { column: 'create_time' },
        { column: 'modify_time' }
      ]
    });
    createTable(db, {
      name: 'Follows',
      columns: {
        follower: 'INTEGER REFERENCES Accounts(rowid)',
        who: 'INTEGER REFERENCES Accounts(rowid)',
      },
      indices: [
        { column: 'follower' },
        { column: 'who' }
      ]
    });
    db.on('drain', function() {
      db.finish();
      callback();
    });
  });
}

exports.connect = connect;
exports.createSchema = createSchema;
exports.createTable = createTable;

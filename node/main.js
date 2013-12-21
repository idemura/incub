var EventEmitter = require('events').EventEmitter;
var express = require('express');
var fs = require('fs');
var http = require('http');
var path = require('path');
var util = require('./util');
var sqlite3 = require('sqlite3');

var config = {
  sqliteDB: 'db.sqlite3',
  hostName: 'localhost',
  port: 4000
};

var emitter = new EventEmitter();
var db, templates;

function openDb(name, callback) {
  return new sqlite3.Database(name, function(err) {
    if (err) {
      print('Failed to open DB: ' + name);
      process.exit(-1);
    } else {
      callback();
    }
  });
}

function readTemplates(dir, callback) {
  var count = 0, tpl = {};
  function read(name) {
    fs.readFile(path.join(dir, name), 'utf8',
                function(err, data) {
                  if (err)
                    throw err;
                  tpl[path.basename(name, '.html')] = data;
                  if (--count === 0) {
                    callback(tpl);
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

// db: Database
// table: Map
// #ret: Void
function dbCreateTable(db, table) {
  var column_def = [];
  for (var k in table.columns) {
    column_def.push(k + ' ' + table.columns[k]);
  }
  var stmt = 'CREATE TABLE ' + table.name + ' (' + column_def.join(', ') + ');';
  db.run(stmt);

  if (table.indices) {
    for (var i = 0, n = table.indices.length; i < n; i++) {
      var c = table.indices[i];
      var name = c.name? c.name: table.name + '_by_' + c.column;
      var stmt = 'CREATE' + (c.unique? ' UNIQUE ': ' ') + 'INDEX ' + name +
                 ' ON ' + table.name + ' (' + c.column + ');';
      db.run(stmt);
    }
  }
}

// name: String
// #ret: Void
function createTables() {
  db.serialize(function () {
    dbCreateTable(db, {
        name: 'accounts',
        columns: {
          id: 'INTEGER PRIMARY KEY AUTOINCREMENT',
          email: 'VARCHAR(95)',
          name: 'VARCHAR(95)',
          firstName: 'VARCHAR(95)',
          birthday: 'VARCHAR(32)',
          picture: 'VARCHAR(255)',
          locale: 'VARCHAR(15)'
        },
        indices: [
          { column: 'email', unique: true },
          { column: 'name' },
        ]
      });
    db.close();
  });
  util.print('DB created.');
}

function serve() {
  var app = express();
  app.get('/', function (req, res) {
    res.setHeader('Content-Type', 'text/html');
    res.send(util.render(templates.main, {title: 'Main', name: 'Igor'}))
  });
  app.listen(config.port, config.hostName);

  var fullUrl = 'http://' + config.hostName + ':' + config.port + '/';
  util.print('Server is running at ' + fullUrl);
}

db = openDb(config.sqliteDB,
  function() {
    if (process.argv.indexOf('--create-tables') >= 0)
      createTables();

    readTemplates('templates', function(tpl) {
      templates = tpl;
      serve();
    });
  });

var http = require('http');
var path = require('path');
var sqlite = require('sqlite3');
var util = require('./util');

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
      util.println(stmt);
      db.run(stmt);
    }
  }
}

// name: String
// #ret: Void
function createDb(name) {
  var db = new sqlite.Database(util.getHome(name));
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
  util.println('DB created.');
}

function main() {
  var ipAddress = '127.0.0.1';
  var port = 4000;
  http.createServer(function (req, res) {
    res.writeHead(200, {'Content-Type': 'text/plain'});
    res.end('Hello World\n');
  }).listen(port, ipAddress);
  util.println('Server running at http://' + ipAddress + ':' + port + '/');
}

// main();
createDb('hello.sqlite');

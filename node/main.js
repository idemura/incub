'use strict';

var auth = require('./auth');
var config = require('./config');
var dlib = require('./dlib');
var express = require('express');
var log = require('./log');
var sqlite3 = require('sqlite3');
var util = require('util');
var view = require('./view');

var templates;

config.gauth.redirectURL = getHostUrl('/gauth');
var gauth = new auth.GAuth(config.gauth, gAuthCB);

function Sessions(db) {
  this.db = db;
}

Sessions.prototype.getSession = function(cookieSession) {
  var now = new Date();
  // db.serialize(function() {
  //   if (cookieSession.sid) {
  //     db.get('SELECT * FROM Sessions WHERE session_id=?;', [cookieSession.sid],
  //            function(err, row) {
  //              if (err)
  //                throw err;
  //              log.print('found row', row);
  //              return row;
  //            });
  //     // select session. if not selected (error or gc-ed by time),
  //     // create new.
  //   } else {
  //     // create new.
  //   }
  // });
}

function getHostUrl(path) {
  var fullHost = 'http://' + config.hostName + ':' + config.port;
  return path? fullHost + path: fullHost;
}

function openDB(callback) {
  var db = new sqlite3.Database(config.sqliteDB, function(err) {
    if (err) {
      // Do not die in page handler.
      dlib.die('DB error.', err);
    } else {
      callback(db);
      db.close();
    }
  });
}

function Handler(fn) {
  this.fn = fn;
  return this;
}

Handler.prototype.handle = function(req, res) {
  var self = this;
  openDB(function(db) {
    self.db = db;
    self.req = req;
    self.res = res;
    self.fn.call(self, req, res);
  });
}

// fn: function(req, res)
function handle(fn) {
  var h = new Handler(fn);
  return function(req, res) {
    h.handle(req, res);
  }
}

// db: Database
// table: Map
// #ret: Void
function dbCreateTable(db, table) {
  var column_def = [];
  for (var k in table.columns) {
    column_def.push(k + ' ' + table.columns[k]);
  }
  var stmt = util.format('CREATE TABLE IF NOT EXISTS %s (%s);', table.name,
                         column_def.join(', '));
  db.run(stmt);

  if (table.indices) {
    for (var i = 0, n = table.indices.length; i < n; i++) {
      var c = table.indices[i];
      var name = c.name? c.name: table.name + '_by_' + c.column;
      var stmt = util.format('CREATE %s INDEX IF NOT EXISTS %s ON %s (%s);',
                             c.unique? 'UNIQUE': '', name, table.name,
                             c.column);
      db.run(stmt);
    }
  }
}

// name: String
// #ret: Void
function createTables() {
  openDB(function(db) {
    db.serialize(function() {
      dbCreateTable(db, {
        name: 'Accounts',
        columns: {
          rowid: 'INTEGER PRIMARY KEY AUTOINCREMENT',
          email: 'VARCHAR(95)',
          gplus_id: 'VARCHAR(35)',
          name: 'VARCHAR(95)',
          given_name: 'VARCHAR(95)',
          picture: 'VARCHAR(255)',
          gender: 'VARCHAR(15)',
          locale: 'VARCHAR(15)'
        },
        indices: [
          { column: 'email', unique: true },
          { column: 'gplus_id', unique: true },
          { column: 'name' },
        ]
      });
      dbCreateTable(db, {
        name: 'Sessions',
        columns: {
          rowid: 'INTEGER PRIMARY KEY AUTOINCREMENT',
          account_id: 'INTEGER REFERENCES Accounts(rowid)',
          session_id: 'VARCHAR(24)',
          create_time: 'INTEGER',
          access_time: 'INTEGER'
        },
        indices: [
          { column: 'account_id' },
          { column: 'session_id', unique: true },
          { column: 'create_time' },
          { column: 'access_time' },
        ]
      });
    });
    log.print('DB created.');
  });
}

function accountsInsertSql(fields) {
  return 'INSERT INTO Accounts (' + fields.join(',') + ') VALUES' +
    ' (' + dlib.repeat('?', fields.length).join(',') + ');';
}

function accountsUpdateSql(fields) {
  function em(s) {
    return s + '=?';
  }
  return 'UPDATE Accounts SET ' + fields.map(em).join(',') + ' WHERE rowid=?;';
}

function project(obj, fields) {
  return fields.map(function(f) {
    return obj[f];
  });
}

function updateAccount(db, u, callback) {
  var user = {email: u.email};
  function update(row) {
    if (row) {
      var fields = ['name', 'given_name', 'picture', 'gender', 'locale'];
      var p = project(u, fields);
      p.push(row.rowid);
      db.run(accountsUpdateSql(fields), p, updateCB);
    } else {
      var fields = ['gplus_id', 'email',
                    'name', 'given_name', 'picture', 'gender', 'locale'];
      var p = project(u, fields);
      p[0] = u.id;
      db.run(accountsInsertSql(fields), p, updateCB);
    }
  }

  function updateCB(err) {
    if (err) {
      log.error('DB SQL error:', err);
      db.run('ROLLBACK;');
    } else {
      db.run('COMMIT;');
      callback(user);
    }
  }

  db.run('BEGIN;', function(err) {
    if (err) {
      log.error('DB SQL error:', err);
    } else {
      db.get('SELECT rowid FROM Accounts WHERE email=?;', [u.email],
             function(err, row) {
        if (err) {
          log.error('DB SQL error:', err);
          db.run('ROLLBACK;');
        } else {
          update(row);
        }
      });
    }
  });
}

function gAuthCB(user, req, res) {
  openDB(function(db) {
    if (user) {
      updateAccount(db, user, function(user) {
        if (user) {
          req.session.email = user.email;
        }
        res.redirect('/');
      });
    } else {
      log.error('Google Auth FAILED.');
      res.redirect('/');
    }
  });
}

var route = {
  index: function(req, res) {
    var data = {title: 'Igor\'s Main', account: null};
    if (req.session.email) {
      data.account = {
        email: req.session.email
      };
    }
    res.setHeader('Content-Type', 'text/html');
    data.gAuthUrl = gauth.authURL();
    res.send(view.render(templates.index, data));
  },

  signOff: function(req, res) {
    // TODO: Ajax
    req.session.email = null;
    res.redirect('/');
  }
};

function serve() {
  var app = express();
  app.use(express.cookieParser('VQPskCSFqMc7rXg4'));
  app.use(express.cookieSession());

  app.get('/', handle(route.index));
  app.get('/gauth', gauth.authResponseHandler());
  app.get('/signoff', route.signOff);

  app.listen(config.port, config.hostName);
  log.print('Server is running at ' + getHostUrl());
}

if (process.argv.indexOf('--create-tables') >= 0) {
  createTables();
}

view.readTemplates('templates', function(err, tpl) {
  if (err) {
    dlib.die('Templates error.', JSON.stringify(err));
  }
  templates = tpl;
  serve();
});

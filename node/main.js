'use strict';

var auth = require('./auth');
var config = require('./config');
var crypto = require('crypto');
var lib = require('./lib');
var express = require('express');
var handler = require('./handler');
var log = require('./log');
var sqlite3 = require('sqlite3');
var util = require('util');
var view = require('./view');

var templates;

config.gauth.redirectURL = getHostUrl('/gauth');
var gauth = new auth.GAuth(config.gauth, gAuthCB);

function Context() {
  this.finish = Context.prototype.finish;
  this.templates = templates;
  return this;
}

Context.prototype.finish = function() {
}

Context.prototype.openDB = function(callback) {
  var self = this;
  self.db = new sqlite3.Database(config.sqliteDB, function(err) {
    if (err) {
      throw err;
    } else {
      var oldFinish = self.finish;
      self.finish = function() {
        self.db.close();
        self.db = null;
        oldFinish.call(self);
      }
      callback(self);
    }
  });
}

Context.prototype.openSession = function(callback) {
  var self = this;

  function invoke(s) {
    self.res.cookie('sid', s.session_id);
    var fields = ['account_id'];
    var oldValues = project(s, fields);
    self.session = s;
    var oldFinish = self.finish;
    self.finish = function() {
      var newValues = project(self.session, fields);
      if (!lib.equals(oldValues, newValues)) {
        newValues.push(self.session.rowid);
        self.db.run(lib.updateSql('Sessions', fields, 'rowid=?'), newValues);
      }
      oldFinish();
    };
    callback(self);
  }

  function create() {
    var sid = crypto.randomBytes(16).toString('base64');
    var fields = ['session_id', 'create_time'];
    self.db.run(lib.insertSql('Sessions', fields), [sid, now],
      function(err) {
        if (err) {
          throw err;
        }
        var s = {
          rowid: this.lastID,
          session_id: sid,
          create_time: now,
          access_time: now
        };
        invoke(s);
      });
  }

  function update(row) {
    row.access_time = now;
    invoke(row);
  }

  var now = Date.now();
  var sid = self.req.cookies.sid;
  if (sid) {
    self.db.get('SELECT * FROM Sessions WHERE session_id=?;', [sid],
      function(err, row) {
        if (err) {
          throw err;
        }
        if (row) {
          update(row);
        } else {
          create();
        }
      });
  } else {
    create();
  }
}

function getHostUrl(path) {
  var fullHost = 'http://' + config.hostName + ':' + config.port;
  return path? fullHost + path: fullHost;
}

// fn: function(ctx, req, res)
function handle(fn) {
  return function(req, res) {
    var ctx = new Context();
    try {
      ctx.openDB(function(ctx) {
        ctx.req = req;
        ctx.res = res;
        ctx.openSession(function(ctx) {
          fn(ctx, req, res);
        });
      });
    } catch (e) {
      log.error('Exception:', e);
      res.send(500);
      ctx.finish();
    }
  };
}

function updateAccount(db, u, callback) {
  var user = {email: u.email};
  function update(row) {
    if (row) {
      user.rowid = row.rowid;
      var fields = ['name', 'given_name', 'picture', 'gender', 'locale'];
      var p = project(u, fields);
      p.push(row.rowid);
      db.run(lib.updateSql('Accounts', fields, 'rowid=?'), p, updateCB);
    } else {
      var fields = ['gplus_id', 'email',
                    'name', 'given_name', 'picture', 'gender', 'locale'];
      var p = project(u, fields);
      p[0] = u.id;
      db.run(lib.insertSql('Accounts', fields), p, function(err) {
        if (!err) {
          user.rowid = this.lastID;
        }
        updateCB(err);
      });
    }
  }

  function updateCB(err) {
    if (err) {
      db.run('ROLLBACK;');
      throw err;
    } else {
      // Race condition here without callback?
      db.run('COMMIT;', function(err) {
        callback(user);
      });
    }
  }

  db.run('BEGIN;', function(err) {
    if (err) {
      log.error('DB SQL error:', err);
    } else {
      db.get('SELECT rowid FROM Accounts WHERE email=?;', [u.email],
             function(err, row) {
        if (err) {
          db.run('ROLLBACK;');
          throw err;
        } else {
          update(row);
        }
      });
    }
  });
}

function gAuthCB(guser, req, res) {
  function render(ctx) {
    ctx.res.redirect('/');
    ctx.finish();
  }

  var fn = handle(function(ctx, req, res) {
    if (guser) {
      updateAccount(ctx.db, guser, function(acc) {
        if (acc) {
          ctx.session.account_id = acc.rowid;
        }
        render(ctx);
      });
    } else {
      log.error('Google Auth FAILED.');
      render(ctx);
    }
  });
  fn(req, res);
}

function serve() {
  var app = express();
  app.use(express.cookieParser());
  app.use(express.json());
  app.use(express.urlencoded());

  app.get('/', handle(function(ctx, req, res) {
    ctx.gauth = gauth;
    handler.main(ctx, req, res);
  }));
  app.get('/gauth', gauth.authResponseHandler());
  app.get('/signoff', handle(handler.signOff));
  app.post('/create', handle(handler.create));

  app.listen(config.port, config.hostName);
  log.print('Server is running at ' + getHostUrl());
}

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

function createTables() {
  var ctx = new Context();
  ctx.openDB(function(ctx) {
    var db = ctx.db;
    db.serialize(function() {
      dbCreateTable(db, {
        name: 'Accounts',
        columns: {
          rowid: 'INTEGER PRIMARY KEY AUTOINCREMENT',
          email: 'TEXT',
          gplus_id: 'TEXT',
          name: 'TEXT',
          given_name: 'TEXT',
          picture: 'TEXT',
          gender: 'TEXT',
          locale: 'TEXT',
          meta: 'TEXT'
        },
        indices: [
          { column: 'email', unique: true },
          { column: 'gplus_id', unique: true },
          { column: 'name' }
        ]
      });
      dbCreateTable(db, {
        name: 'Sessions',
        columns: {
          rowid: 'INTEGER PRIMARY KEY AUTOINCREMENT',
          session_id: 'TEXT',
          account_id: 'INTEGER REFERENCES Accounts(rowid)',
          create_time: 'INTEGER',
          access_time: 'INTEGER',
          meta: 'TEXT'
        },
        indices: [
          { column: 'account_id' },
          // Should be unique, but collisions have tiny probability.
          { column: 'session_id' },
          { column: 'create_time' },
          { column: 'access_time' }
        ]
      });
      dbCreateTable(db, {
        name: 'Posts',
        columns: {
          rowid: 'INTEGER PRIMARY KEY AUTOINCREMENT',
          account_id: 'INTEGER REFERENCES Accounts(rowid)',
          create_time: 'INTEGER',
          modify_time: 'INTEGER',
          text: 'TEXT',
          meta: 'TEXT'
        },
        indices: [
          { column: 'account_id' },
          { column: 'create_time' },
          { column: 'modify_time' }
        ]
      });
    });
    log.print('DB created.');
  });
}

if (process.argv.indexOf('--create-tables') >= 0) {
  createTables();
}

view.readTemplates('templates', function(err, tpl) {
  if (err) {
    lib.die('Templates error.', JSON.stringify(err));
  }
  templates = tpl;
  serve();
});

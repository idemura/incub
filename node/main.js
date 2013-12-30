'use strict';

var auth = require('./auth');
var config = require('./config');
var crypto = require('crypto');
var db = require('./db');
var lib = require('./lib');
var express = require('express');
var handler = require('./handler');
var log = require('./log');
var util = require('util');
var view = require('./view');

function Context() {
  this.finalizers = [];
  this.templates = templates;
  return this;
}

Context.prototype.addFinalizer = function(fin) {
  this.finalizers.push(fin);
}

Context.prototype.finish = function() {
  var finalizers = this.finalizers;
  for (var i = finalizers.length; i-- > 0; ) {
    finalizers[i]();
  }
}

Context.prototype.openDB = function(callback) {
  var self = this;
  db.connect(config.db_url, function(err, db) {
    if (err) {
      throw err;
    }
    self.db = db;
    self.addFinalizer(db.finish.bind(db));
    callback(self);
  });
}

Context.prototype.openSession = function(callback) {
  var self = this;

  function invoke(s) {
    self.res.cookie('sid', s.session_id);
    var fields = ['account_id'];
    var oldValues = lib.project(s, fields);
    self.session = s;
    self.addFinalizer(function() {
      var newValues = lib.project(self.session, fields);
      if (!lib.equals(oldValues, newValues)) {
        newValues.push(self.session.rowid);
        self.db.query(lib.updateSql('Sessions', fields, 'rowid=?'), newValues);
      }
    });
    callback(self);
  }

  function create() {
    var sid = crypto.randomBytes(16).toString('base64');
    var fields = ['session_id', 'create_time'];
    self.db.query(lib.insertSql('Sessions', fields), [sid, now],
      function(err, res) {
        if (err) {
          throw err;
        }
        var s = {
          rowid: res.rows[0].rowid,
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
    self.db.query('SELECT * FROM Sessions WHERE session_id=?;', [sid],
      function(err, res) {
        if (err) {
          throw err;
        }
        if (res.rows.length > 0) {
          update(res.rows[0]);
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
      var p = lib.project(u, fields);
      p.push(row.rowid);
      db.query(lib.updateSql('Accounts', fields, 'rowid=?'), p, updateCB);
    } else {
      var fields = ['gplus_id', 'email',
                    'name', 'given_name', 'picture', 'gender', 'locale'];
      var p = lib.project(u, fields);
      p[0] = u.id;
      db.query(lib.insertSql('Accounts', fields), p, function(err, res) {
        if (!err) {
          user.rowid = res.rows[0].rowid;
        }
        updateCB(err);
      });
    }
  }

  function updateCB(err) {
    if (err) {
      db.query('ROLLBACK;');
      throw err;
    } else {
      // Race condition here without callback?
      db.query('COMMIT;', function(err) {
        callback(user);
      });
    }
  }

  db.query('BEGIN;', function(err) {
    if (err) {
      log.error('DB SQL error:', err);
    } else {
      db.query('SELECT rowid FROM Accounts WHERE email=?;', [u.email],
               function(err, res) {
        if (err) {
          db.query('ROLLBACK;');
          throw err;
        } else {
          update(res.rows[0]);
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

function createTables() {
  db.connect(config.db_url, function(err, db) {
    if (err) {
      log.error('DB error:', err);
      return;
    }
    dbCreateTable(db, {
      name: 'Accounts',
      columns: {
        rowid: 'SERIAL UNIQUE',
        email: 'TEXT',
        gplus_id: 'TEXT',
        name: 'TEXT',
        given_name: 'TEXT',
        picture: 'TEXT',
        gender: 'TEXT',
        locale: 'TEXT',
      },
      indices: [
        // { column: 'rowid', unique: true },
        { column: 'email', unique: true },
        { column: 'gplus_id', unique: true },
        { column: 'name' }
      ]
    });
    dbCreateTable(db, {
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
    dbCreateTable(db, {
      name: 'Posts',
      columns: {
        rowid: 'SERIAL UNIQUE',
        account_id: 'INTEGER REFERENCES Accounts(rowid)',
        create_time: 'BIGINT',
        modify_time: 'BIGINT',
        text: 'TEXT',
        meta: 'TEXT'
      },
      indices: [
        // { column: 'rowid', unique: true },
        { column: 'account_id' },
        { column: 'create_time' },
        { column: 'modify_time' }
      ]
    });
    db.on('drain', function() {
      db.finish();
      log.print('DB created.');
      process.exit();
    });
  });
}

var templates, gauth;
config.read(function(err) {
  config.gauth.redirectURL = getHostUrl('/gauth');
  gauth = new auth.GAuth(config.gauth, gAuthCB);
  if (err) {
    log.error('Config error:', err);
    return;
  }

  if (process.argv.indexOf('--create-tables') >= 0) {
    createTables();
  } else {
    view.readTemplates('templates', function(err, tpl) {
      if (err) {
        log.die('Templates error:', err);
      }
      templates = tpl;
      serve();
    });
  }
});

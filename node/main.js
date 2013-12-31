'use strict';

var auth = require('./auth');
var config = require('./config');
var crypto = require('crypto');
var db = require('./db');
var lib = require('./lib');
var express = require('express');
var handler = require('./handler');
var log = require('./log');
var path = require('path');
var util = require('util');
var view = require('./view');

function Context() {
  this.finalizers = [];
  this.session = {};
  this.sessionMetaModified = 0;
  this.view = {};
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
      return log.fatal(err);
    }
    self.db = db;
    self.addFinalizer(db.finish.bind(db));
    callback();
  });
}

Context.prototype.saveSession = function(session, saveMeta) {
  var keys = ['account_id', 'create_time', 'access_time'];
  var values = lib.values(session, keys);
  if (saveMeta) {
    keys.push('meta');
    values.push(JSON.stringify(session.meta));
  }
  values.push(session.rowid);
  this.db.query(lib.updateSql('Sessions', keys, 'rowid=?'), values,
                function(err, dbres) {
    if (err) {
      return log.fatal(err);
    }
  });
}

Context.prototype.openSession = function(callback) {
  var self = this;

  function invoke(session) {
    self.res.cookie('sid', session.session_id);
    self.session = session;
    self.addFinalizer(function() {
      self.saveSession(self.session, self.sessionMetaModified > 0);
    });
    callback();
  }

  function create() {
    var sid = crypto.randomBytes(16).toString('base64');
    var keys = ['session_id', 'create_time'];
    self.db.query(lib.insertSql('Sessions', keys), [sid, now],
      function(err, dbres) {
        if (err) {
          return log.fatal(err);
        }
        var s = {
          rowid: dbres.rows[0].rowid,
          session_id: sid,
          create_time: now,
          access_time: now
        };
        invoke(s);
      });
  }

  function update(row) {
    row.access_time = now;
    row.meta = row.meta? JSON.parse(row.meta): {};
    invoke(row);
  }

  var now = Date.now();
  var sid = self.req.cookies.sid;
  if (sid) {
    self.db.query('SELECT * FROM Sessions WHERE session_id=?;', [sid],
      function(err, dbres) {
        if (err) {
          return log.fatal(err);
        }
        if (dbres.rows.length > 0) {
          update(dbres.rows[0]);
        } else {
          create();
        }
      });
  } else {
    create();
  }
}

Context.prototype.setSessionMeta = function(k, v) {
  this.session.meta[k] = v;
  this.sessionMetaModified++;
}

Context.prototype.getSessionMeta = function(k, def) {
  return this.session.meta[k] || def;
}

Context.prototype.responseHTML = function(tpl) {
  var res = this.res;
  switch (path.extname(tpl)) {
    case '.html':
      res.set('Content-Type', 'text/html');
      break;
    case '.json':
      res.set('Content-Type', 'application/json');
      break;
    default:
      res.set('Content-Type', 'text/plain');
  }
  var rendered = view.render(tpl, this.view);
  res.send(rendered? rendered: 500);
}

Context.prototype.responseJSON = function(data) {
  var res = this.res;
  res.set('Content-Type', 'application/json');
  res.send(JSON.stringify(data));
}

function getHostUrl(path) {
  var fullHost = 'http://' + config.hostName + ':' + config.port;
  return path? fullHost + path: fullHost;
}

// fn: function(ctx, req, res)
function handle(fn) {
  return function(req, res) {
    var ctx = new Context();
    ctx.openDB(function() {
      ctx.req = req;
      ctx.res = res;
      ctx.openSession(function() {
        fn(ctx, req, res);
      });
    });
  };
}

function updateAccount(db, u, callback) {
  var user = {email: u.email};
  function update(row) {
    if (row) {
      user.rowid = row.rowid;
      var keys = ['name', 'given_name', 'picture', 'gender', 'locale'];
      var p = lib.values(u, keys);
      p.push(row.rowid);
      db.query(lib.updateSql('Accounts', keys, 'rowid=?'), p, updateCB);
    } else {
      var groupName = u.email === 'igor.demura@gmail.com'? 'admin': 'user';
      u.pgroup = Context.prototype.groups[groupName].rowid;
      var keys = ['gplus_id', 'pgroup', 'email',
                  'name', 'given_name', 'picture', 'gender', 'locale'];
      var p = lib.values(u, keys);
      p[0] = u.id;
      db.query(lib.insertSql('Accounts', keys), p, function(err, dbres) {
        if (!err) {
          user.rowid = dbres.rows[0].rowid;
        }
        updateCB(err);
      });
    }
  }

  function updateCB(err) {
    if (err) {
      db.query('ROLLBACK;');
      log.fatal(err);
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
               function(err, dbres) {
        if (err) {
          db.query('ROLLBACK;');
          log.fatal(err);
        } else {
          update(dbres.rows[0]);
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

function readGroups(callback) {
  db.connect(config.db_url, function(err, db) {
    if (err) {
      return log.fatal(err);
    }
    db.query('SELECT * FROM Groups;', [], function(err, dbres) {
      if (err) {
        return log.fatal(err);
      }
      callback(dbres.rows);
    });
  });
}

function serve() {
  var app = express();
  app.use(express.cookieParser());
  app.use(express.json());
  app.use(express.urlencoded());

  app.get('/', handle(handler.main));
  app.get('/gauth', Context.prototype.gauth.authResponseHandler());
  app.get('/signoff', handle(handler.signOff));
  app.get('/feedtime', handle(handler.feedTime));
  app.get('/feed', handle(handler.feed));
  app.post('/create', handle(handler.create));

  app.listen(config.port, config.hostName);
  log.print('Server is running at ' + getHostUrl());
}

config.read(function(err) {
  if (err) {
    log.die('Config error:', err);
    return;
  }
  config.gauth.redirectURL = getHostUrl('/gauth');
  Context.prototype.gauth = new auth.GAuth(config.gauth, gAuthCB);

  if (process.argv.indexOf('--create-tables') >= 0) {
    db.createSchema(function() {
      log.print('DB created.');
    });
  } else {
    readGroups(function(groups) {
      var groupsMap = {};
      // Since group names are unique, insert into a map.
      for (var i = 0; i < groups.length; i++) {
        groups[i].privileges = JSON.parse(groups[i].privileges);
        groupsMap[groups[i].name] = groups[i];
      }
      Context.prototype.groups = groupsMap;

      view.readTemplates('templates', function(err) {
        if (err) {
          log.die('Templates error:', err);
        }
        serve();
      });
    });
  }
});

'use strict';

var config = require('./config');
var dlib = require('./dlib');
var EventEmitter = require('events').EventEmitter;
var express = require('express');
var fs = require('fs');
var log = require('./log');
var path = require('path');
var sqlite3 = require('sqlite3');
var util = require('util');
var view = require('./view');

var emitter = new EventEmitter();
var db, templates;

function getHostUrl(path) {
  var fullHost = 'http://' + config.hostName + ':' + config.port;
  return path? fullHost + path: fullHost;
}

function openDb(name, callback) {
  return new sqlite3.cached.Database(name, function(err) {
    if (err) {
      dlib.die('DB error: ' + JSON.stringify(err));
    } else {
      callback();
    }
  });
}

function readTemplates(dir, callback) {
  var count = 0, tpl = {};
  function read(name) {
    fs.readFile(path.join(dir, name), 'utf8', function(err, data) {
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
    // db.close();
  });
  log.print('DB created.');
}

function gAuthUrl() {
  var param = {
    response_type: 'code',
    access_type: 'online',
    redirect_uri: getHostUrl('/gauth'),
    client_id: config.gauth.clientID,
    scope: config.gauth.scopes
  };
  return config.gauth.authURL + '?' + dlib.urlEncode(param);
}

function gAuthExchangeCode(code, callback) {
  var form = {
    code: code,
    client_id: config.gauth.clientID,
    client_secret: config.gauth.clientSecret,
    redirect_uri: getHostUrl('/gauth'),
    grant_type: 'authorization_code'
  };
  dlib.postForm(config.gauth.tokenURL, form, function(err, body) {
    if (err) {
      log.error('Net (gAuthExchangeCode):', err);
      callback(err, null);
    } else {
      var gAuthResponse = JSON.parse(body.toString('utf-8'));
      callback(null, gAuthResponse);
    }
  });
}

function updateAccount(ui, callback) {
  var user = {email: ui.email};
  function update(err, row) {
    if (err) {
      updateDone(err);
      return;
    }
    if (row) {
      db.run('UPDATE Accounts SET name=?, given_name=?, picture=?, gender=?, locale=? WHERE rowid=?',
             [ui.name, ui.given_name, ui.picture, ui.gender, ui.locale, row.rowid],
             updateDone);
    } else {
      var fields = ['email', 'gplus_id', 'name', 'given_name', 'picture', 'gender', 'locale'];
      var ph = dlib.repeat('?', fields.length).join(',');  // Placeholders
      db.run('INSERT INTO Accounts (' + fields.join(',') + ') VALUES (' + ph + ')',
             [ui.email, ui.id, ui.name, ui.given_name, ui.picture, ui.gender, ui.locale],
             updateDone);
    }
  }
  function updateDone(err) {
    if (!err) {
      user.id = this.lastID;
    }
    callback(user);
  }

  db.serialize(function() {
    db.get('SELECT rowid FROM Accounts WHERE email=?', [ui.email], update);
  });
}

function gUserInfo(token, callback) {
  var param = {
    alt: 'json',
    access_token: token
  };
  var addr = config.gauth.userInfoURL + '?' + dlib.urlEncode(param);
  dlib.request(addr, {}, function(err, body) {
    if (err) {
      log.error('Net (gUserInfo):', err);
      callback(null);
    } else {
      var ui = JSON.parse(body.toString('utf-8'));
      updateAccount(ui, callback);
    }
  });
}

function gAuthFailed(res) {
  log.error('Google Auth FAILED.');
  res.redirect('/');
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
    data.gAuthUrl = gAuthUrl();
    res.send(view.render(templates.index, data));
  },

  gAuth: function(req, res) {
    gAuthExchangeCode(req.param('code'), function(err, token) {
      if (err) {
        gAuthFailed(res);
      } else {
        gUserInfo(token.access_token, function(user) {
          if (user) {
            req.session.email = user.email;
            res.redirect('/');
          } else {
            gAuthFailed(res);
          }
        });
      }
    });
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
  app.get('/', route.index);
  app.get('/gauth', route.gAuth);
  app.get('/signoff', route.signOff);
  app.listen(config.port, config.hostName);
  log.print('Server is running at ' + getHostUrl());
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

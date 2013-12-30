'use strict';

var lib = require('./lib');
var log = require('./log');
var view = require('./view');

function getAccount(db, rowid, callback) {
  db.query('SELECT * FROM Accounts WHERE rowid=?;', [rowid],
           function(err, dbres) {
    if (err) {
      throw err;
    } else {
      callback(dbres.rows[0]);
    }
  });
}

function getPostsOf(db, account_id, callback) {
  db.query('SELECT * FROM Posts WHERE account_id=? ORDER BY modify_time DESC;',
           [account_id], function(err, dbres) {
    if (err) {
      throw err;
    } else {
      callback(dbres.rows);
    }
  });
}

function main(ctx, req, res) {
  function renderNoAuth() {
    data.gauthURL = ctx.gauth.authURL();
    ctx.res.send(view.render(ctx.templates.main_noauth, data));
    ctx.finish();
  }

  var data = {
    title: 'Igor\'s Main',
  };
  if (ctx.session.account_id) {
    getAccount(ctx.db, ctx.session.account_id, function(account) {
      if (account) {
        data.account = {email: account.email};
        data.posts = [];
        getPostsOf(ctx.db, account.rowid, function(rows) {
          data.posts = rows.map(function(r) {
            return {text: r.text};
          });
          ctx.res.send(view.render(ctx.templates.main, data));
          ctx.finish();
        });
      } else {
        renderNoAuth();
      }
    });
  } else {
    renderNoAuth();
  }
}

function signOff(ctx, req, res) {
  ctx.session.account_id = null;
  res.redirect('/');
  ctx.finish();
}

function withAuth(ctx, callback, callbackUnauthorized) {
  callbackUnauthorized = callbackUnauthorized || function() {
    res.redirect('/');
    ctx.finish();
  };
  if (ctx.session.account_id) {
    getAccount(ctx.db, ctx.session.account_id, function(account) {
      if (account) {
        callback(account);
      } else {
        callbackUnauthorized();
      }
    });
  } else {
    callbackUnauthorized();
  }
}

function create(ctx, req, res) {
  withAuth(ctx, function(account) {
    var text = req.body.text;
    if (text && 0 < text.length && text.length < 256) {
      var now = Date.now();
      var post = {
        account_id: account.rowid,
        create_time: now,
        modify_time: now,
        text: text
      };
      var keys = lib.keys(post);
      var values = lib.values(post, keys);
      ctx.db.query(lib.insertSql('Posts', keys), values, function(err, dbres) {
        if (err) {
          throw err;
        }
        res.redirect('/');
        ctx.finish();
      });
    } else {
      ctx.finish();
    }
  });
}

exports.create = create;
exports.main = main;
exports.signOff = signOff;

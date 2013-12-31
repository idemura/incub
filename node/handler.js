'use strict';

var lib = require('./lib');
var log = require('./log');
var view = require('./view');

function getAccount(db, rowid, callback) {
  db.query('SELECT * FROM Accounts WHERE rowid=?;', [rowid],
           function(err, dbres) {
    if (err) {
      log.fatal(err);
    } else {
      callback(dbres.rows[0]);
    }
  });
}

function getPostsOf(db, account_id, begin, callback) {
  db.query('SELECT * FROM Posts WHERE account_id=? AND rowid>? ' +
           'ORDER BY modify_time DESC LIMIT 80;',
           [account_id, begin], function(err, dbres) {
    if (err) {
      log.fatal(err);
    } else {
      callback(dbres.rows);
    }
  });
}

function mainAuthorized(ctx, req, res, account) {
  var data = {
    title: 'Igor\'s Main',
    account: account,
    posts: []
  };
  getPostsOf(ctx.db, account.rowid, 0, function(rows) {
    data.posts = rows;
    var maxRowID = 0;
    rows.forEach(function(r) {
      if (r.rowid > maxRowID) {
        maxRowID = r.rowid;
      }
    })
    ctx.setSessionMeta('lastFeedRow', maxRowID);
    ctx.render('main.html', data);
    ctx.finish();
  });
}

function main(ctx, req, res) {
  function renderNoAuth() {
    var data = {
      title: 'Igor\'s Main',
      gauthURL: ctx.gauth.authURL()
    };
    ctx.render('main_noauth.html', data);
    ctx.finish();
  }

  var authAccountID = ctx.session.account_id;
  if (authAccountID) {
    getAccount(ctx.db, authAccountID, function(account) {
      if (account) {
        mainAuthorized(ctx, req, res, account);
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
    res.send(401);
    ctx.finish();
  };
  var authAccountID = ctx.session.account_id;
  if (authAccountID) {
    getAccount(ctx.db, authAccountID, function(account) {
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
    var now = Date.now(), text = req.body.text;
    var sinceLastPost = now - ctx.getSessionMeta('lastPostTime', 0);
    if (text && 0 < text.length && text.length < 256 && sinceLastPost > 3000) {
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
          log.fatal(err);
        }
        ctx.setSessionMeta('lastPostTime', now);
        res.send('<p>OK</p>');  // DUMMY.
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

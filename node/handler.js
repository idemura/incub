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

function getPostsOf(db, account_id, since, limit, callback) {
  db.query('SELECT * FROM Posts WHERE account_id=? AND modify_time>? ' +
           'ORDER BY modify_time DESC LIMIT ' + limit + ';',
           [account_id, since], function(err, dbres) {
    if (err) {
      log.fatal(err);
    } else {
      callback(dbres.rows);
    }
  });
}

function getFeedLast(feed) {
  return (feed[0] && feed[0].modify_time) || 0;
}

function mainAuthorized(ctx) {
  ctx.view.account = ctx.account;
  ctx.view.posts = [];
  getPostsOf(ctx.db, ctx.account.rowid, 0, 80, function(rows) {
    ctx.view.posts = rows;
    ctx.view.feedLast = getFeedLast(rows);
    log.trace('feed last', ctx.view.feedLast);
    ctx.renderHTML('main.html');
    ctx.finish();
  });
}

function main(ctx, req, res) {
  ctx.view.title = 'Igor\'s Main';
  withAuth(ctx, mainAuthorized, function(ctx) {
    ctx.view.gauthURL = ctx.gauth.authURL();
    ctx.renderHTML('main_noauth.html');
    ctx.finish();
  });
}

function signOff(ctx, req, res) {
  ctx.session.account_id = null;
  res.redirect('/');
  ctx.finish();
}

function withAuth(ctx, callback, callbackUnauthorized) {
  function noAuthDefault(ctx) {
    res.send(401);
    ctx.finish();
  };

  callbackUnauthorized = callbackUnauthorized || noAuthDefault;
  var authAccountID = ctx.session.account_id;
  if (authAccountID) {
    getAccount(ctx.db, authAccountID, function(account) {
      if (account) {
        ctx.account = account;
        callback(ctx);
      } else {
        callbackUnauthorized(ctx);
      }
    });
  } else {
    callbackUnauthorized(ctx);
  }
}

function create(ctx, req, res) {
  withAuth(ctx, function() {
    var now = Date.now(), text = req.body.text;
    var sinceLastPost = now - ctx.getSessionMeta('lastPostTime', 0);
    if (text && 0 < text.length && text.length < 256 && sinceLastPost > 3000) {
      var post = {
        account_id: ctx.account.rowid,
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

function feedLast(ctx, req, res) {
  withAuth(ctx, function() {
    // TODO: Anti DDOS.
    log.trace('query: ', req.query);
    var since = req.query.since || 0;
    log.trace('since: ', since);
    getPostsOf(ctx.db, ctx.account.rowid, since, 1, function(rows) {
      ctx.renderJSON({
        feedLast: getFeedLast(rows),
        count: rows.length
      });
      ctx.finish();
    });
  });
}

exports.create = create;
exports.feedLast = feedLast;
exports.main = main;
exports.signOff = signOff;

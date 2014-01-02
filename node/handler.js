'use strict';

var lib = require('./lib');
var log = require('./log');
var view = require('./view');

function getAccount(db, rowid, callback) {
  db.query('SELECT * FROM Accounts WHERE rowid=?;', [rowid],
           function(err, dbres) {
    if (err) {
      return log.fatal(err);
    }
    callback(dbres.rows[0]);
  });
}

var FEED_MAX = 80;
function getPostsOf(db, account_id, since, limit, callback) {
  db.query('SELECT * FROM Posts WHERE account_id=? AND modify_time>? ' +
           'ORDER BY modify_time DESC LIMIT ' + limit + ';',
           [account_id, since], function(err, dbres) {
    if (err) {
      return log.fatal(err);
    }
    callback(dbres.rows);
  });
}

function getFeedTime(feed) {
  return (feed[0] && feed[0].modify_time) || 0;
}

function mainAuthorized(ctx) {
  ctx.view.account = ctx.account;
  ctx.view.feed = [];
  getPostsOf(ctx.db, ctx.account.rowid, 0, FEED_MAX, function(rows) {
    ctx.view.feed = rows;
    ctx.view.feedTime = getFeedTime(rows);
    ctx.responseHTML('main.html');
    ctx.finish();
  });
}

function main(ctx, req, res) {
  withAuth(ctx, mainAuthorized, function(ctx) {
    ctx.view.gauthURL = ctx.gauth.authURL();
    ctx.responseHTML('main_noauth.html');
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
          res.send(500);
        } else {
          ctx.setSessionMeta('lastPostTime', now);
          res.send('<p>OK</p>');  // DUMMY.
        }
        ctx.finish();
      });
    } else {
      ctx.finish();
    }
  });
}

function feedTime(ctx, req, res) {
  withAuth(ctx, function() {
    // TODO: Anti DDOS.
    var since = req.query.since || 0;
    getPostsOf(ctx.db, ctx.account.rowid, since, 1, function(rows) {
      ctx.responseJSON({
        feedTime: getFeedTime(rows),
        count: rows.length
      });
      ctx.finish();
    });
  });
}

function feed(ctx, req, res) {
  withAuth(ctx, function() {
    // TODO: Anti DDOS.
    var since = req.query.since || 0;
    getPostsOf(ctx.db, ctx.account.rowid, since, FEED_MAX, function(rows) {
      ctx.view.feed = rows;
      ctx.responseJSON({
        feedTime: getFeedTime(rows),
        html: view.render('feed.html', ctx.view)
      });
      ctx.finish();
    });
  });
}

exports.create = create;
exports.feed = feed;
exports.feedTime = feedTime;
exports.main = main;
exports.signOff = signOff;

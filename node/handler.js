'use strict';

var lib = require('./lib');
var log = require('./log');
var util = require('util');
var view = require('./view');

function getAccount(ctx, callback) {
  var accountID = ctx.session.account_id;
  if (accountID) {
    ctx.db.query('SELECT * FROM Accounts WHERE rowid=?;', [accountID],
                 function(err, dbres) {
      if (err) {
        log.fatal(err);
        ctx.finish(500);
      } else {
        callback(dbres.rows.length > 0? dbres.rows[0]: null);
      }
    });
  } else {
    callback(null);
  }
}

var FEED_MAX = 80;
function getPostsOf(ctx, since, limit, callback) {
  var accountID = ctx.session.account_id;
  ctx.db.query('SELECT * FROM Posts WHERE account_id=? AND modify_time>? ' +
               'ORDER BY modify_time DESC LIMIT ' + limit + ';',
               [accountID, since], function(err, dbres) {
    if (err) {
      log.fatal(err);
      ctx.finish(500);
    } else {
      callback(dbres.rows);
    }
  });
}

function getFeedTime(feed) {
  return (feed[0] && feed[0].modify_time) || 0;
}

function mainAuthorized(ctx) {
  ctx.view.account = ctx.account;
  ctx.view.feed = [];
  var since = 0, limit = FEED_MAX;
  getPostsOf(ctx, since, limit, function(feed) {
    ctx.view.feed = feed;
    ctx.view.feedTime = getFeedTime(feed);
    ctx.responseHTML('main.html');
    ctx.finish();
  });
}

function main(ctx) {
  withAuth(ctx, mainAuthorized, function(ctx) {
    ctx.view.gauthURL = ctx.gauth.authURL();
    ctx.responseHTML('main_noauth.html');
    ctx.finish();
  });
}

function logout(ctx) {
  ctx.session.account_id = null;
  ctx.res.redirect('/');
  ctx.finish();
}

function withAuth(ctx, callback, callbackUnauthorized) {
  getAccount(ctx, function(account) {
    if (account && account.userid !== null) {
      ctx.account = account;
      callback(ctx);
    } else {
      if (callbackUnauthorized) {
        callbackUnauthorized(ctx);
      } else {
        ctx.finish(401);
      }
    }
  });
}

function create(ctx) {
  withAuth(ctx, function() {
    var now = Date.now(), text = ctx.req.body.text;
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
          ctx.finish(500);
        } else {
          ctx.setSessionMeta('lastPostTime', now);
          ctx.res.send('<p>OK</p>');  // DUMMY. Do JSON with error code.
          ctx.finish();
        }
      });
    } else {
      ctx.finish();
    }
  });
}

function userID(ctx) {
  getAccount(ctx, function(account) {
    if (account && account.userid === null) {
      ctx.view.account = ctx.account = account;
      ctx.responseHTML('userid.html');
    } else {
      ctx.res.redirect('/');
    }
    ctx.finish();
  });
}

function createUserID(ctx) {
  function setUserID(userid, account_id, callback) {
    var ERRFMT_LONG = '%s is too long (max 24).';
    var ERRFMT_SHORT = '%s is too short (min 3).';
    var ERRFMT_CHARS = 'User should contain alpha, digit, - or _: %s';
    var ERRFMT_RENAME = 'User already has ID. Rename is not allowed.';
    var ERRFMT_EXISTS = 'User ID %s already exists.';

    userid = userid.trim();
    if (userid.length > 24) {
      return callback({userid: userid, format: ERRFMT_LONG});
    }
    if (userid.length < 3) {
      return callback({userid: userid, format: ERRFMT_SHORT});
    }
    if (!/^[a-zA-Z0-9\-_]+$/.test(userid)) {
      return callback({userid: userid, format: ERRFMT_CHARS});
    }

    ctx.db.query('BEGIN;', function(err, dbres) {
      if (err) {
        ctx.db.query('ROLLBACK;');
        return ctx.finish(500);
      }
      ctx.db.query('SELECT userid FROM Accounts WHERE rowid=?',
                   [account_id], function(err, dbres) {
        if (err) {
          ctx.db.query('ROLLBACK;');
          return ctx.finish(500);
        }
        if (dbres.rows[0] && dbres.rows[0].userid) {
          ctx.db.query('ROLLBACK;');
          return callback({userid: userid, format: ERRFMT_RENAME});
        }
        ctx.db.query('SELECT rowid FROM Accounts WHERE userid=?', [userid],
                     function(err, dbres) {
          if (err) {
            ctx.db.query('ROLLBACK;');
            return ctx.finish(500);
          }
          if (dbres.rows[0]) {
            return callback({userid: userid, format: ERRFMT_EXISTS});
          }
          ctx.db.query(lib.updateSql('Accounts', ['userid'], 'rowid=?'),
                       [userid, account_id], function(err, dbres) {
            if (err) {
              ctx.db.query('ROLLBACK;');
              return ctx.finish(500);
            }
            ctx.db.query('COMMIT;');
            callback();
          });
        });
      });
    });
  }

  if (!ctx.session.account_id) {
    return ctx.finish(401);
  }
  setUserID(ctx.req.body.userid, ctx.session.account_id, function(err) {
    if (err) {
      ctx.responseJSON({ok: false,
                        useridError: util.format(err.format, err.userid)});
    } else {
      ctx.responseJSON({ok: true});
    }
    ctx.finish();
  });
}

function feedTime(ctx) {
  withAuth(ctx, function() {
    // TODO: Anti DDOS.
    var since = ctx.req.query.since || 0, limit = 1;
    getPostsOf(ctx, since, limit, function(feed) {
      ctx.responseJSON({
        feedTime: getFeedTime(feed),
        count: feed.length
      });
      ctx.finish();
    });
  });
}

function feed(ctx) {
  withAuth(ctx, function() {
    // TODO: Anti DDOS.
    var since = ctx.req.query.since || 0, limit = FEED_MAX;
    getPostsOf(ctx, since, limit, function(feed) {
      ctx.view.feed = feed;
      ctx.responseJSON({
        feedTime: getFeedTime(feed),
        html: view.render('feed.html', ctx.view)
      });
      ctx.finish();
    });
  });
}

exports.create = create;
exports.createUserID = createUserID;
exports.feed = feed;
exports.feedTime = feedTime;
exports.logout = logout;
exports.main = main;
exports.userID = userID;

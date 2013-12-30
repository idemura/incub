'use strict';

var lib = require('./lib');
var log = require('./log');
var view = require('./view');

function getAccount(db, rowid, callback) {
  var fields = ['email', 'name', 'given_name', 'picture', 'gender', 'locale'];
  db.query('SELECT ' + fields.join(',') + ' FROM Accounts WHERE rowid=?;',
           [rowid], function(err, res) {
    if (err) {
      throw err;
    } else {
      callback(res.rows[0]);
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
        ctx.res.send(view.render(ctx.templates.main, data));
        ctx.finish();
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

function create(ctx, req, res) {
  res.redirect('/');
  ctx.finish();
}

exports.create = create;
exports.main = main;
exports.signOff = signOff;

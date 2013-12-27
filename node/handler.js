'use strict';

var lib = require('./lib');
var log = require('./log');
var view = require('./view');

function getAccount(db, rowid, callback) {
  var fields = ['email', 'name', 'given_name', 'picture', 'gender', 'locale'];
  db.get('SELECT ' + fields.join(',') + ' FROM Accounts WHERE rowid=?;',
         [rowid], function(err, row) {
    if (err) {
      throw err;
    } else {
      callback(row);
    }
  });
}

function main(ctx, req, res) {
  function render(ctx) {
    ctx.res.send(view.render(ctx.templates.main, data));
    ctx.finish();
  }

  var data = {
    title: 'Igor\'s Main',
    gauthURL: ctx.gauth.authURL()
  };
  if (ctx.session.account_id) {
    getAccount(ctx.db, ctx.session.account_id, function(account) {
      if (account) {
        data.account = {email: account.email};
      }
      render(ctx);
    });
  } else {
    render(ctx);
  }
}

function signOff(ctx, req, res) {
  ctx.session.account_id = null;
  res.redirect('/');
  ctx.finish();
}

exports.main = main;
exports.signOff = signOff;

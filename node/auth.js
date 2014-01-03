'use strict';

var lib = require('./lib');
var log = require('./log');

function GAuth(config, callback) {
  this.config = config;
  this.callback = callback;
}

GAuth.prototype.authURL = function() {
  var param = {
    response_type: 'code',
    access_type: 'online',
    redirect_uri: this.config.redirectURL,
    client_id: this.config.clientID,
    scope: this.config.scopes
  };
  return 'https://accounts.google.com/o/oauth2/auth?' + lib.urlEncode(param);
};

GAuth.prototype.authResponseHandler = function() {
  var self = this;
  return function(req, res) {
    var form = {
      code: req.param('code'),
      client_id: self.config.clientID,
      client_secret: self.config.clientSecret,
      redirect_uri: self.config.redirectURL,
      grant_type: 'authorization_code'
    };
    lib.postForm('https://accounts.google.com/o/oauth2/token', form,
                  function(err, body) {
      if (err) {
        self.callback(null, req, res);
      } else {
        var token = JSON.parse(body.toString('utf-8'));
        gUserInfo.call(self, token.access_token, function(user) {
          if (user) {
            self.callback(user, req, res);
          } else {
            self.callback(null, req, res);
          }
        });
      }
    });
  };
}

function gUserInfo(token, callback) {
  var param = {
    alt: 'json',
    access_token: token
  };
  var addr = 'https://www.googleapis.com/oauth2/v2/userinfo?' +
             lib.urlEncode(param);
  lib.request(addr, {}, function(err, body) {
    if (err) {
      callback(null);
    } else {
      callback(JSON.parse(body.toString('utf-8')));
    }
  });
}

exports.GAuth = GAuth;

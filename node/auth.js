'use strict';

var dlib = require('./dlib');

function GAuth(config, callback) {
  this.config = config;
  this.callback = callback;
  return this;
}

GAuth.prototype.authURL = function() {
  var param = {
    response_type: 'code',
    access_type: 'online',
    redirect_uri: this.config.redirectURL,
    client_id: this.config.clientID,
    scope: this.config.scopes
  };
  return 'https://accounts.google.com/o/oauth2/auth?' + dlib.urlEncode(param);
};

GAuth.prototype.authResponseHandler = function() {
  function handler(req, res) {
    var form = {
      code: req.param('code'),
      client_id: self.config.clientID,
      client_secret: self.config.clientSecret,
      redirect_uri: self.config.redirectURL,
      grant_type: 'authorization_code'
    };
    dlib.postForm('https://accounts.google.com/o/oauth2/token', form,
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
  }

  var self = this;
  return handler.bind(self);
}

function gUserInfo(token, callback) {
  var param = {
    alt: 'json',
    access_token: token
  };
  var addr = 'https://www.googleapis.com/oauth2/v2/userinfo?' +
             dlib.urlEncode(param);
  dlib.request(addr, {}, function(err, body) {
    if (err) {
      callback(null);
    } else {
      callback(JSON.parse(body.toString('utf-8')));
    }
  });
}

exports.GAuth = GAuth;

'use strict';

module.exports = {
  sqliteDB: 'db.sqlite3',
  hostName: 'localhost',
  port: 4000,

  gauth: {
    authURL: 'https://accounts.google.com/o/oauth2/auth',
    tokenURL: 'https://accounts.google.com/o/oauth2/token',
    userInfoURL: 'https://www.googleapis.com/oauth2/v2/userinfo',
    clientID: '484563975237.apps.googleusercontent.com',
    clientSecret: 'XyjfDwvcQUA8xYO9n9iW0iW2',
    scopes: ['https://www.googleapis.com/auth/userinfo.email',
             'https://www.googleapis.com/auth/userinfo.profile']
  }
};

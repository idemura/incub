var mustache = require('mustache');
var sqlite3 = require('sqlite3');

var HOME = process.env.HOME || process.env.HOMEPATH || process.env.USERPROFILE;

module.exports.print = function() {
  console.log.apply(null, arguments);
}

module.exports.render = function(tpl, view) {
  return mustache.render(tpl, view);
}

module.exports.openDb = function(name, callback) {
  return new sqlite3.Database(name,
    function(err) {
      if (err) {
        print('Failed to open DB: ' + name);
        process.exit(-1);
      } else {
        callback();
      }
    });
}

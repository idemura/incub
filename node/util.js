var path = require('path');

module.exports.println = function() {
  console.log.apply(null, arguments);
}

module.exports.getHome = function(name) {
  var home = process.env.HOME || process.env.HOMEPATH ||
             process.env.USERPROFILE;
  if (name) {
    return path.join(home, name);
  } else {
    return home;
  }
}

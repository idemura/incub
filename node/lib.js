'use strict';

var http = require('http');
var https = require('https');
var log = require('./log');
var qstr = require('querystring');
var url = require('url');
var util = require('util');

var HOME = process.env.HOME || process.env.HOMEPATH || process.env.USERPROFILE;

function assign(name, val) {
  var old = this[name];
  this[name] = val;
  return old;
}

function equals(lh, rh) {
  if (lh instanceof Array && rh instanceof Array) {
    if (lh.length != rh.length) {
      return false;
    }
    for (var i = 0, n = lh.length; i < n; i++) {
      if (lh[i] !== rh[i]) {
        return false;
      }
    }
    return true;
  }
  return lh === rh;
}

function repeat(x, n) {
  var xs = [];
  while (n--) {
    xs[n] = x;
  }
  return xs;
}

function repeatStr(s, n) {
  var s = '';
  while (n--) {
    s += x;
  }
  return s;
}

function typeOf(value) {
    var t = typeof value;
    if (t === 'object') {
        if (value) {
            if (value instanceof Array) {
                t = 'array';
            }
        } else {
            t = 'null';
        }
    }
    return t;
}

function urlEncode(obj) {
  var acc = [];
  for (var k in obj) {
    var v = obj[k];
    if (v instanceof Array)
      var str = v.map(qstr.escape).join('+');
    else
      var str = qstr.escape(v);
    acc.push(k + '=' + str);
  }
  return acc.join('&');
}

function request(address, options, callback) {
  var fields = url.parse(address);
  options.hostname = fields.hostname;
  options.path = fields.path;

  var chunks = [];
  var module = fields.protocol === 'https:'? https: http;
  var req = module.request(options, function(res) {
    res.on('data', function(chunk) {
      chunks.push(chunk);
    });
    res.on('end', function() {
      var size = 0;
      for (var i = 0; i < chunks.length; i++) {
        size += chunks[i].length;
      }
      var buffer = new Buffer(size);
      var offset = 0;
      for (var i = 0; i < chunks.length; i++) {
        chunks[i].copy(buffer, offset);
        offset += chunks[i].length;
      }
      callback(null, buffer);
    });
  });
  if (options.data) {
    req.write(options.data);
  }
  req.end();
  req.on('error', function(err) {
    callback(err, null);
  });
}

function postForm(address, form, callback) {
  var data = urlEncode(form);
  var options = {
    method: 'POST',
    data: data,
    headers: {
      'Content-Type': 'application/x-www-form-urlencoded',
      'Content-Length': data.length
    }
  };
  request(address, options, callback);
}

function insertSql(table, fields) {
  return util.format('INSERT INTO %s (%s) VALUES (%s) RETURNING rowid;',
    table, fields.join(','), repeat('?', fields.length).join(','));
}

function updateSql(table, fields, where) {
  function em(s) {
    return s + '=?';
  }
  var stmt = 'UPDATE ' + table + ' SET ' + fields.map(em).join(',');
  if (stmt) {
    stmt += ' WHERE ' + where;
  }
  stmt += ';';
  return stmt;
}

function project(obj, fields) {
  return fields.map(function(f) {
    return obj[f];
  });
}

function getCmdLineArg(name, def) {
  var argv = process.argv, val;
  for (var i = 1; i < argv.length; i++) {
    if (argv[i] === name) {
      val = argv[i + 1];
    }
  }
  if (val)
    return val;
  else
    return def;
}

exports.assign = assign;
exports.equals = equals;
exports.insertSql = insertSql;
exports.getCmdLineArg = getCmdLineArg;
exports.postForm = postForm;
exports.project = project;
exports.repeat = repeat;
exports.repeatStr = repeatStr;
exports.request = request;
exports.typeOf = typeOf;
exports.updateSql = updateSql;
exports.urlEncode = urlEncode;

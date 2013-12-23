'use strict';

var http = require('http');
var https = require('https');
var log = require('./log');
var qstr = require('querystring');
var url = require('url');
var util = require('util');

var HOME = process.env.HOME || process.env.HOMEPATH || process.env.USERPROFILE;

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

function die(msg) {
  log.error('FATAL ERROR. Process is exiting:');
  log.error(msg);
  process.exit(1);
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

exports.die = die;
exports.postForm = postForm;
exports.repeat = repeat;
exports.repeatStr = repeatStr;
exports.request = request;
exports.urlEncode = urlEncode;

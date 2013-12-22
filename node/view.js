'use strict';

var mustache = require('mustache');

(function(mmap) {
  function render(tpl, view) {
    return mustache.render(tpl, view);
  }

  mmap.render = render;
}(module.exports));

// Generated by BUCKLESCRIPT VERSION 4.0.5, PLEASE EDIT WITH CARE
'use strict';

var Belt_List = require("bs-platform/lib/js/belt_List.js");

function path($$location) {
  var pathname = $$location.pathname;
  var match = pathname[pathname.length - 1 | 0];
  var raw = match === "/" ? pathname.slice(0, -1) : pathname;
  return Belt_List.fromArray(raw.split("/"));
}

function search($$location) {
  var raw = $$location.search;
  return raw.slice(1);
}

function hash($$location) {
  var raw = $$location.hash;
  return raw.slice(1);
}

function make(path, hash, search) {
  return /* record */[
          /* path */path,
          /* hash */hash,
          /* search */search
        ];
}

function toUrl(route) {
  return Belt_List.reduce(route[/* path */0], "/", (function (prim, prim$1) {
                return prim + prim$1;
              }));
}

exports.path = path;
exports.search = search;
exports.hash = hash;
exports.make = make;
exports.toUrl = toUrl;
/* No side effect */
// Generated by BUCKLESCRIPT VERSION 4.0.0, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Belt_List = require("bs-platform/lib/js/belt_List.js");
var BsHistory = require("bs-history/src/BsHistory.bs.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var CreateHashHistory = require("history/createHashHistory");

var historyOpts = {
  basename: ""
};

var router = CreateHashHistory.default(historyOpts);

function path($$location) {
  var pathname = $$location.pathname;
  console.log("pathname", pathname);
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

function makeRoute(path, hash, search) {
  return /* record */[
          /* path */path,
          /* hash */hash,
          /* search */search
        ];
}

function fromRouteDefault(_, _$1) {
  return /* NoUpdate */0;
}

function toRouteDefault() {
  console.log("toRoute default");
  return /* NoTransition */1;
}

function updateDefault() {
  return /* NoUpdate */0;
}

function viewDefault() {
  return "View not implemeneted";
}

function program(debug) {
  return /* record */[
          /* debug */debug,
          /* fromRoute */fromRouteDefault,
          /* toRoute */toRouteDefault,
          /* update */updateDefault,
          /* view */viewDefault
        ];
}

function programStateWrapper(initState, looper) {
  var currentState = /* record */[/* contents */initState];
  var runner = function () {
    var update = Curry._1(looper[/* dispatch */1], currentState[0]);
    var nextState = update ? update[0] : currentState[0];
    var routeUpdate = Curry._1(looper[/* updateRoute */3], /* record */[
          /* previous */currentState[0],
          /* next */nextState
        ]);
    if (typeof routeUpdate === "number") {
      routeUpdate === 0;
    } else if (routeUpdate.tag) {
      var url = Belt_List.reduce(routeUpdate[0][/* path */0], "/", (function (prim, prim$1) {
              return prim + prim$1;
            }));
      console.log(url);
      router.replace(url);
    } else {
      var url$1 = Belt_List.reduce(routeUpdate[0][/* path */0], "/", (function (prim, prim$1) {
              return prim + prim$1;
            }));
      console.log(url$1);
      router.push(url$1);
    }
    currentState[0] = nextState;
    var self = /* record */[
      /* state */nextState,
      /* send */runner
    ];
    Curry._1(looper[/* render */4], self);
    return /* () */0;
  };
  BsHistory.listen((function ($$location, action) {
            var route = function () {
              return /* record */[
                      /* path */path($$location),
                      /* hash */hash($$location),
                      /* search */search($$location)
                    ];
            };
            var routeAction = action !== 4003185 ? (
                action >= 893009402 ? (console.log("push"), /* Push */1) : (console.log("replace"), /* Replace */3)
              ) : (console.log("pop"), /* Pop */2);
            console.log("listener");
            var update = Curry._2(looper[/* getFromRoute */2], routeAction, route(/* () */0));
            var nextState = update ? update[0] : currentState[0];
            currentState[0] = nextState;
            var self = /* record */[
              /* state */nextState,
              /* send */runner
            ];
            Curry._1(looper[/* render */4], self);
            return /* () */0;
          }))(router);
  var self_000 = /* state */currentState[0];
  var self = /* record */[
    self_000,
    /* send */runner
  ];
  Curry._1(looper[/* start */0], self);
  return /* () */0;
}

function loop(update, view, toRoute, fromRoute, enqueueRender) {
  return /* record */[
          /* start */(function (self) {
              return Curry._1(enqueueRender, Curry._1(view, self));
            }),
          /* dispatch */Curry.__1(update),
          /* getFromRoute */Curry.__2(fromRoute),
          /* updateRoute */Curry.__1(toRoute),
          /* render */(function (self) {
              return Curry._1(enqueueRender, Curry._1(view, self));
            })
        ];
}

function startup(program, renderer) {
  var initRoute = function () {
    var $$location = router.location;
    return /* record */[
            /* path */path($$location),
            /* hash */hash($$location),
            /* search */search($$location)
          ];
  };
  var match = Curry._2(program[/* fromRoute */1], /* Init */0, initRoute(/* () */0));
  var initState = match ? match[0] : Pervasives.failwith("Must init a state");
  var looper = loop(program[/* update */3], program[/* view */4], program[/* toRoute */2], program[/* fromRoute */1], renderer);
  programStateWrapper(initState, looper);
  return /* () */0;
}

var routerProgram = program;

var defaultRoute = /* record */[
  /* path : :: */[
    "",
    /* [] */0
  ],
  /* hash */"",
  /* search */""
];

exports.historyOpts = historyOpts;
exports.router = router;
exports.path = path;
exports.search = search;
exports.hash = hash;
exports.makeRoute = makeRoute;
exports.defaultRoute = defaultRoute;
exports.fromRouteDefault = fromRouteDefault;
exports.toRouteDefault = toRouteDefault;
exports.updateDefault = updateDefault;
exports.viewDefault = viewDefault;
exports.program = program;
exports.programStateWrapper = programStateWrapper;
exports.loop = loop;
exports.startup = startup;
exports.routerProgram = routerProgram;
/* router Not a pure module */

type t('state) = {
  debug: string,
  fromRoute: (routeAction, route) => update('state),
  toRoute: previousAndNextState('state) => routeUpdate,
  update: 'state => update('state),
  view: self('state) => ReasonReact.reactElement,
}
and route = {
  path: list(string),
  hash: string,
  search: string,
}
and routeAction =
  | Init
  | Push
  | Pop
  | Replace
and self('state) = {
  state: 'state,
  send: unit => unit,
}
and previousAndNextState('state) = {
  previous: 'state,
  next: 'state,
}
and update('state) =
  | Update('state)
  /* | SideEffects(self('state, 'action) => unit) */
  /* | UpdateWithSideEffects('state, self('state, 'action) => unit) */
  | NoUpdate
and routeUpdate =
  | Push(route)
  | Replace(route)
  | Pop
  | NoTransition;

let historyOpts =
  BsHistory.makeHashHistoryOptions(
    ~basename="",
    /* ~initialEntries=[|"/"|],
       ~initialIndex=0,
       ~keyLength=6, */
  );
let router = BsHistory.createHashHistory(historyOpts);

let path = location => {
  let pathname = BsHistory.Location.pathname(location);
  Js.log2("pathname", pathname);
  let raw =
    switch (Js.String.get(pathname, Js.String.length(pathname) - 1)) {
    | "/" => Js.String.slice(~from=0, ~to_=-1, pathname)
    | _ => pathname
    };

  raw |> Js.String.split("/") |> Belt.List.fromArray;
};

let search = location => {
  let raw = BsHistory.Location.search(location);
  raw |> Js.String.sliceToEnd(~from=1);
};
let hash = location => {
  let raw = BsHistory.Location.hash(location);
  raw |> Js.String.sliceToEnd(~from=1);
};

let makeRoute = (~path, ~hash, ~search) => {path, hash, search};
let defaultRoute = {path: [""], hash: "", search: ""};

let fromRouteDefault: (routeAction, route) => update('state) =
  (_action, _route) => NoUpdate;
let toRouteDefault: previousAndNextState('state) => routeUpdate =
  _prevAndNext => {
    Js.log("toRoute default");
    NoTransition;
  };

let updateDefault: 'state => update('state) = _state => NoUpdate;

let viewDefault: 'state => ReasonReact.reactElement =
  _self => ReasonReact.string("View not implemeneted");

let program: string => t('state) =
  debug => {
    let template = {
      debug,
      fromRoute: fromRouteDefault,
      toRoute: toRouteDefault,
      update: updateDefault,
      view: viewDefault,
    };
    template;
  };

type loop('state) = {
  start: self('state) => unit,
  dispatch: 'state => update('state),
  getFromRoute: (routeAction, route) => update('state),
  updateRoute: previousAndNextState('state) => routeUpdate,
  render: self('state) => unit,
};

let programStateWrapper: ('state, loop('state)) => unit =
  (initState, looper) => {
    let currentState = ref(initState);

    let rec runner = () => {
      let update = looper.dispatch(currentState^);
      let nextState =
        switch (update) {
        | Update(nextState) => nextState
        | NoUpdate => currentState^
        };

      let routeUpdate =
        looper.updateRoute({previous: currentState^, next: nextState});
      let _ =
        switch (routeUpdate) {
        | Push(route) =>
          let url = Belt.List.reduce(route.path, "/", (++));
          Js.log(url);
          BsHistory.push(url, router);
        | Replace(route) =>
          let url = Belt.List.reduce(route.path, "/", (++));
          Js.log(url);
          BsHistory.replace(url, router);
        | Pop => /* TODO: goBack */ ()
        | NoTransition => ()
        };

      currentState := nextState;

      let self = {send: runner, state: nextState};
      looper.render(self);
      ();
    };

    /* looper.updateRoute(currentState^); */
    let unlisten =
      BsHistory.listen(
        (location, action) => {
          let route = () => {
            path: path(location),
            search: search(location),
            hash: hash(location),
          };

          let routeAction =
            switch (action) {
            | `Push => Push
            | `Pop => Pop
            | `Replace => Replace
            };
          Js.log("listener");

          let _ = looper.getFromRoute(routeAction, route());
          ();
        },
        router,
      );

    let self = {send: runner, state: currentState^};
    looper.start(self);
    ();
  };

let loop:
  (
    ~update: 'state => update('state),
    ~view: self('state) => ReasonReact.reactElement,
    ~toRoute: previousAndNextState('state) => routeUpdate,
    ~fromRoute: (routeAction, route) => update('state),
    ~enqueueRender: ReasonReact.reactElement => unit
  ) =>
  loop('state) =
  (~update, ~view, ~toRoute, ~fromRoute, ~enqueueRender) => {
    start: self => {
      let initView = view(self);
      enqueueRender(initView);
    },
    dispatch: state => {
      let nextState = update(state);

      nextState;
    },
    getFromRoute: (action, route) => {
      let next = fromRoute(action, route);

      next;
    },
    updateRoute: prevAndNextState => {
      let update = toRoute(prevAndNextState);

      update;
    },
    render: self => {
      let nextView = view(self);

      enqueueRender(nextView);
    },
  };

let startup: (t('state), ReasonReact.reactElement => unit) => unit =
  (program, renderer) => {
    let initRoute = () => {
      let location = BsHistory.location(router);
      {
        path: path(location),
        search: search(location),
        hash: hash(location),
      };
    };

    let initState =
      switch (program.fromRoute(Init, initRoute())) {
      | Update(state) => state
      /* | UpdateWithSideEffects(state, effect) => (state, Some(effect)) */
      | NoUpdate => failwith("Must init a state")
      /* | SideEffects(_effect) => failwith("Must init a state") */
      };

    /* let result =
       switch (program.toRoute({previous: initState, next: initState})) {
       | NoTransition => None
       | _ =>
         Some(
           "toRoute should result in no transition when called with initial state.",
         )
       }; */

    let looper =
      loop(
        ~update=program.update,
        ~view=program.view,
        ~toRoute=program.toRoute,
        ~fromRoute=program.fromRoute,
        ~enqueueRender=renderer,
      );

    let _ = programStateWrapper(initState, looper);
    ();
  };

let routerProgram: string => t('state) = debug => program(debug);

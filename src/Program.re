type t('action, 'state, 'view) = {
  debug: string,
  fromRoute: (routeAction, Route.t) => update('state),
  toRoute: previousAndNextState('state) => routeUpdate,
  update: ('action, 'state) => update('state),
  view: self('action, 'state) => 'view,
}
and routeAction =
  | Init
  | Push
  | Pop
  | Replace
and self('action, 'state) = {
  state: 'state,
  send: 'action => unit,
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
  | Push(Route.t)
  | Replace(Route.t)
  | Pop
  | NoTransition
and loop('action, 'state) = {
  start: self('action, 'state) => unit,
  dispatch: ('action, 'state) => update('state),
  getFromRoute: (routeAction, Route.t) => update('state),
  updateRoute: previousAndNextState('state) => unit,
  render: self('action, 'state) => unit,
};

let historyOpts =
  BsHistory.makeHashHistoryOptions(
    ~basename="",
    /* ~initialEntries=[|"/"|],
       ~initialIndex=0,
       ~keyLength=6, */
  );
let router = BsHistory.createHashHistory(historyOpts);

let getRoute = location =>
  Route.make(
    ~path=Route.path(location),
    ~search=Route.search(location),
    ~hash=Route.hash(location),
  );

let defaultRoute = Route.make(~path=[""], ~hash="", ~search="");

let fromRouteDefault: (routeAction, Route.t) => update('state) =
  (_action, _route) => NoUpdate;
let toRouteDefault: previousAndNextState('state) => routeUpdate =
  _prevAndNext => NoTransition;

let updateDefault: ('action, 'state) => update('state) =
  (_action, _state) => NoUpdate;

let viewDefault: 'state => 'view =
  _self => failwith("Must assign a view method");

let program: string => t('action, 'state, 'view) =
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

let programStateWrapper: ('state, loop('action, 'state)) => unit =
  (initState, looper) => {
    let currentState = ref(initState);

    let rec makeSelf = state => {send: runner, state}
    and runner = action => {
      let update = looper.dispatch(action, currentState^);
      let nextState =
        switch (update) {
        | Update(nextState) => nextState
        | NoUpdate => currentState^
        };

      let _ = looper.updateRoute({previous: currentState^, next: nextState});

      currentState := nextState;

      looper.render(makeSelf(nextState));
      ();
    };

    let unlisten =
      BsHistory.listen(
        (location, action) => {
          let routeAction =
            switch (action) {
            | `Push =>
              Js.log("listener: push");
              Push;
            | `Pop =>
              Js.log("listener: pop");
              Pop;
            | `Replace =>
              Js.log("listener: replace");
              Replace;
            };

          let update = looper.getFromRoute(routeAction, getRoute(location));
          let nextState =
            switch (update) {
            | Update(nextState) => nextState
            | NoUpdate => currentState^
            };

          currentState := nextState;
          looper.render(makeSelf(nextState));
          ();
        },
        router,
      );

    looper.start(makeSelf(currentState^));
    ();
  };

let loop:
  (
    ~update: ('action, 'state) => update('state),
    ~view: self('action, 'state) => 'view,
    ~toRoute: previousAndNextState('state) => routeUpdate,
    ~fromRoute: (routeAction, Route.t) => update('state),
    ~enqueueRender: 'view => unit
  ) =>
  loop('action, 'state) =
  (~update, ~view, ~toRoute, ~fromRoute, ~enqueueRender) => {
    start: self => {
      let initView = view(self);
      enqueueRender(initView);
    },
    dispatch: (action, state) => {
      let nextState = update(action, state);

      nextState;
    },
    getFromRoute: (action, route) => {
      let next = fromRoute(action, route);

      next;
    },
    updateRoute: prevAndNextState => {
      let update = toRoute(prevAndNextState);

      let _ =
        switch (update) {
        | Push(route) => BsHistory.push(Route.toUrl(route), router)
        | Replace(route) => BsHistory.replace(Route.toUrl(route), router)
        | Pop => /* TODO: goBack */ ()
        | NoTransition => ()
        };

      ();
    },
    render: self => {
      let nextView = view(self);

      enqueueRender(nextView);
    },
  };

let startup: (t('action, 'state, 'view), 'view => unit) => unit =
  (program, renderer) => {
    let location = BsHistory.location(router);
    let initState =
      switch (program.fromRoute(Init, getRoute(location))) {
      | Update(state) => state
      /* | UpdateWithSideEffects(state, effect) => (state, Some(effect)) */
      | NoUpdate => failwith("Must init a state")
      /* | SideEffects(_effect) => failwith("Must init a state") */
      };

    /* TOOD: DevMode only */
    let _ =
      switch (program.toRoute({previous: initState, next: initState})) {
      | NoTransition => ()
      | _ =>
        failwith(
          "toRoute should result in no transition when called with initial state.",
        )
      };

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

let routerProgram: string => t('action, 'state, 'view) =
  debug => program(debug);

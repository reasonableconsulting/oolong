type t('action, 'state, 'view) = {
  debug: string,
  fromRoute: (Router.Action.t, Route.t) => update('action, 'state),
  toRoute: previousAndNextState('state) => routeUpdate,
  update: ('action, 'state) => update('action, 'state),
  view: self('action, 'state) => 'view,
}
and self('action, 'state) = {
  state: 'state,
  send: 'action => unit,
}
and previousAndNextState('state) = {
  previous: 'state,
  next: 'state,
}
and update('action, 'state) =
  | Update('state)
  /* | SideEffects(self('state, 'action) => unit) */
  | UpdateWithSideEffects('state, self('action, 'state) => unit)
  | NoUpdate
and routeUpdate =
  | Push(Route.t)
  | Replace(Route.t)
  | Pop
  | NoTransition
and loop('action, 'state) = {
  init: unit => 'state,
  start: self('action, 'state) => unit,
  listen: ((BsHistory.Location.t, Router.Action.t) => unit) => unit,
  dispatch: ('action, 'state) => update('action, 'state),
  getFromRoute: (Router.Action.t, Route.t) => update('action, 'state),
  updateRoute: previousAndNextState('state) => unit,
  render: self('action, 'state) => unit,
};

let getRoute = location =>
  Route.make(
    ~path=Route.path(location),
    ~search=Route.search(location),
    ~hash=Route.hash(location),
  );

let defaultRoute = Route.make(~path=[""], ~hash="", ~search="");

let fromRouteDefault: (Router.Action.t, Route.t) => update('action, 'state) =
  (_action, _route) => NoUpdate;
let toRouteDefault: previousAndNextState('state) => routeUpdate =
  _prevAndNext => NoTransition;

let updateDefault: ('action, 'state) => update('action, 'state) =
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
        | UpdateWithSideEffects(nextState, sideEffect) =>
          sideEffect(makeSelf(nextState));
          nextState;
        | Update(nextState) => nextState
        | NoUpdate => currentState^
        };

      let _ = looper.updateRoute({previous: currentState^, next: nextState});

      currentState := nextState;

      looper.render(makeSelf(nextState));
      ();
    };

    /* TODO: Group with subscriptions? */
    looper.listen((location, action) => {
      let update = looper.getFromRoute(action, getRoute(location));
      let nextState =
        switch (update) {
        | UpdateWithSideEffects(nextState, sideEffect) =>
          sideEffect(makeSelf(nextState));
          nextState;
        | Update(nextState) => nextState
        | NoUpdate => currentState^
        };

      currentState := nextState;
      looper.render(makeSelf(nextState));
      ();
    });

    looper.start(makeSelf(currentState^));
    ();
  };

let loop:
  (
    ~router: Router.t,
    ~update: ('action, 'state) => update('action, 'state),
    ~view: self('action, 'state) => 'view,
    ~toRoute: previousAndNextState('state) => routeUpdate,
    ~fromRoute: (Router.Action.t, Route.t) => update('action, 'state),
    ~enqueueRender: 'view => unit
  ) =>
  loop('action, 'state) =
  (~router, ~update, ~view, ~toRoute, ~fromRoute, ~enqueueRender) => {
    let _ = ();

    {
      init: _ => {
        let location = Router.getCurrent(router);
        let initState =
          switch (fromRoute(Init, getRoute(location))) {
          | Update(state) => state
          /* TODO: This needs access to the makeSelf/runner */
          /* | UpdateWithSideEffects(nextState, sideEffect) =>
             sideEffect(makeSelf(nextState));
             nextState; */
          | NoUpdate => failwith("Must init a state")
          /* | SideEffects(_effect) => failwith("Must init a state") */
          };

        /* TOOD: DevMode only */
        let _ =
          switch (toRoute({previous: initState, next: initState})) {
          | NoTransition => ()
          | _ =>
            failwith(
              "toRoute should result in no transition when called with initial state.",
            )
          };

        initState;
      },
      listen: callback => {
        /* TODO: Unlisten on shutdown */
        let unlisten = Router.listen(callback, router);
        ();
      },
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
  };

let defaultRouter = Router.memory();

let startup:
  (~router: Router.t=?, t('action, 'state, 'view), 'view => unit) => unit =
  (~router=defaultRouter, program, renderer) => {
    let looper =
      loop(
        ~router,
        ~update=program.update,
        ~view=program.view,
        ~toRoute=program.toRoute,
        ~fromRoute=program.fromRoute,
        ~enqueueRender=renderer,
      );

    let initState = looper.init();

    let _ = programStateWrapper(initState, looper);
    ();
  };

let routerProgram: string => t('action, 'state, 'view) =
  debug => program(debug);

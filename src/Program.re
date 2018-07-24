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
  handle:
    'payload .
    (('payload, self('action, 'state)) => unit, 'payload) => unit,

}
and previousAndNextState('state) = {
  previous: 'state,
  next: 'state,
}
and update('action, 'state) =
  | Update('state)
  /* | SideEffects(self('state, 'action) => unit) */
  | UpdateWithSideEffects('state, sideEffect('action, 'state))
  | NoUpdate
and routeUpdate =
  | Push(Route.t)
  | Replace(Route.t)
  | Pop
  | NoTransition
and sideEffect('action, 'state) = self('action, 'state) => unit
and loop('action, 'state) = {
  init: unit => (option('state), option(sideEffect('action, 'state))),
  /* start: self('action, 'state) => unit, */
  listen: ((BsHistory.Location.t, Router.Action.t) => unit) => unit,
  dispatch:
    ('action, 'state) =>
    (option('state), option(sideEffect('action, 'state))),
  getFromRoute:
    (Router.Action.t, Route.t) =>
    (option('state), option(sideEffect('action, 'state))),
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

let programStateWrapper:
  ('state, option(sideEffect('action, 'state)), loop('action, 'state)) =>
  unit =
  (initState, maybeEffect, looper) => {
    let currentState = ref(initState);
    let loopCounter = ref(0);

    let rec makeSelf = state => {
      send: runner,
      state,
      handle: (fn, data) => fn(data, makeSelf(state)),
    }
    and handle = (maybeNextState, maybeEffect) => {
      let _ =
        switch (maybeNextState) {
        | Some(nextState) when nextState != currentState^ =>
          /* TODO: This should be an effect */
          looper.updateRoute({previous: currentState^, next: nextState});

          Js.log3("update state", currentState^, nextState);
          currentState := nextState;
        | Some(_) => ()
        | None => ()
        };

      switch (maybeEffect) {
      | Some(effect) =>
        effect(makeSelf(currentState^));
        ();
      | None => ()
      };
    }
    and runner = action => {
      loopCounter := loopCounter^ + 1;

      let (maybeNextState, maybeEffect) =
        looper.dispatch(action, currentState^);

      handle(maybeNextState, maybeEffect);

      loopCounter := loopCounter^ - 1;
      /* TODO: requestAnimationFrame on this? */
      if (loopCounter^ == 0) {
        looper.render(makeSelf(currentState^));
      };

      ();
    };

    /* TODO: Group with subscriptions? */
    looper.listen((location, action) => {
      let (maybeNextState, maybeEffect) =
        looper.getFromRoute(action, getRoute(location));

      handle(maybeNextState, maybeEffect);
    });

    switch (maybeEffect) {
    | Some(effect) =>
      effect(makeSelf(currentState^));
      ();
    | None => ()
    };

    looper.render(makeSelf(currentState^));
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

    let previousRoute = ref(getRoute(Router.getCurrent(router)));

    let updateToOptions = update =>
      switch (update) {
      | Update(nextState) => (Some(nextState), None)
      | UpdateWithSideEffects(nextState, sideEffect) => (
          Some(nextState),
          Some(sideEffect),
        )
      | NoUpdate => (None, None)
      /* | SideEffects(_effect) => failwith("Must init a state") */
      };

    {
      init: _ => {
        /* TODO: Compare with previousLocation? */
        let location = Router.getCurrent(router);
        updateToOptions(fromRoute(Init, getRoute(location)));
      },
      listen: callback => {
        /* TODO: Unlisten on shutdown */
        let unlisten = Router.listen(callback, router);
        ();
      },
      dispatch: (action, state) => updateToOptions(update(action, state)),
      getFromRoute: (action, route) => {
        let location = getRoute(Router.getCurrent(router));
        if (previousRoute^ != location) {
          updateToOptions(fromRoute(action, route));
        } else {
          (None, None);
        };
      },
      updateRoute: prevAndNextState => {
        let update = toRoute(prevAndNextState);

        let _ =
          switch (update) {
          | Push(route) when previousRoute^ != route =>
            previousRoute := route;
            BsHistory.push(Route.toUrl(route), router);
            ();
          | Push(_) => ()
          | Replace(route) when previousRoute^ != route =>
            previousRoute := route;
            BsHistory.replace(Route.toUrl(route), router);
          | Replace(_) => ()
          | Pop => /* TODO: goBack */ ()
          | NoTransition => ()
          };

        ();
      },
      render: self => {
        Js.log2("render", self.state);
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

    let (maybeInitState, maybeInitEffect) = looper.init();
    let initState =
      switch (maybeInitState) {
      | Some(state) => state
      | None => failwith("`fromRoute` must return an initial state")
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

    let _ = programStateWrapper(initState, maybeInitEffect, looper);
    ();
  };

let routerProgram: string => t('action, 'state, 'view) =
  debug => program(debug);

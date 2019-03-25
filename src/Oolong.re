type self('action, 'state) = {
  state: 'state,
  send: 'action => unit,
  handle:
    'payload.
    (('payload, self('action, 'state)) => unit, 'payload) => unit,

}
and sideEffect('action, 'state) = self('action, 'state) => unit
and routeState('action, 'state) =
  | State('state)
  | StateWithSideEffects('state, sideEffect('action, 'state))
and update('action, 'state) =
  | Update('state)
  | NoUpdate
  | UpdateWithSideEffects('state, sideEffect('action, 'state))
  | SideEffects(sideEffect('action, 'state))
and routerUpdate('action, 'state) =
  | Push('state)
  | PushWithSideEffects('state, sideEffect('action, 'state))
  | Replace('state)
  | ReplaceWithSideEffects('state, sideEffect('action, 'state))
  | Pop
  | PopWithSideEffects(sideEffect('action, 'state));

let rec makeSelf = (send, getState) => {
  send,
  state: getState(),
  handle: (fn, data) => fn(data, makeSelf(send, getState)),
};

let state = state => State(state);
let stateWithSideEffects = (state, sideEffect) =>
  StateWithSideEffects(state, sideEffect);

module Url = Oolong_Internals.Url;
module Route = Oolong_Internals.Route;
module Router = Oolong_Internals.Router;
module RouterAction = Oolong_Internals.RouterAction;

module BasicProgram = {
  type t('action, 'state, 'view) = {
    debugName: string,
    initialState: unit => 'state,
    reducer: ('action, 'state) => update('action, 'state),
    render: self('action, 'state) => 'view,
    subscriptions: 'state => list(sideEffect('action, 'state)),
  };

  let make = debugName => {
    debugName,
    initialState: () => failwith("Must implement initialState method."),
    reducer: (_action, _state) => failwith("Must implement reducer method."),
    render: _self => failwith("Must implement render method."),
    subscriptions: _state => [],
  };

  let run = (program, callback) => {
    let state = ref(program.initialState());

    let loopCounter = ref(0);

    let getState = () => state^;

    let rec send = action => {
      loopCounter := loopCounter^ + 1;
      let previous = getState();
      let update = program.reducer(action, previous);
      let () =
        switch (update) {
        | Update(nextState) =>
          state := nextState;
          ();
        | NoUpdate => ()
        | UpdateWithSideEffects(nextState, sideEffect) =>
          state := nextState;
          sideEffect(makeSelf(send, getState));
          ();
        | SideEffects(sideEffect) =>
          sideEffect(makeSelf(send, getState));
          ();
        };
      let next = getState();

      loopCounter := loopCounter^ - 1;
      /* TODO: requestAnimationFrame on this? */
      if (loopCounter^ === 0 && next !== previous) {
        let () = callback(program.render(makeSelf(send, getState)));
        ();
      };
    };

    program.subscriptions(getState())
    ->Belt.List.forEach(subscription =>
        subscription(makeSelf(send, getState))
      );

    let () = callback(program.render(makeSelf(send, getState)));
    ();
  };
};

module RouterProgram = {
  type t('action, 'state, 'view) = {
    /* This is literally to hide the "you've defined all the properties" warning */
    debugName: string,
    serializeState: 'state => string,
    init: (list(string), string, string) => routeState('action, 'state),
    fromRoute: (RouterAction.t, 'state) => routeState('action, 'state),
    toRoute: ('action, 'state) => routerUpdate('action, 'state),
    render: self('action, 'state) => 'view,
    subscriptions: 'state => list(sideEffect('action, 'state)),
  };

  /* TODO: default state serializer */
  /* TODO: maybe hide serialize state somehow? */
  let make = (~serializeState, debugName) => {
    debugName,
    serializeState,
    init: (_path, _search, _hash) => failwith("Must implement init method."),
    fromRoute: (_routerAction, _state) =>
      failwith("Must implement fromRoute method."),
    toRoute: (_action, _state) => failwith("Must implement reducer method."),
    render: _self => failwith("Must implement render method."),
    subscriptions: _state => [],
  };

  type routerAction('action, 'state) =
    | UserAction('action)
    | RouteChanged(RouterAction.t);

  type routerState('state) =
    | UserState('state);

  let getUserState =
    fun
    | UserState(userState) => userState;

  let getUserSelf = ourSelf => {
    let send = action => ourSelf.send(UserAction(action));
    let getState = () => getUserState(ourSelf.state);

    makeSelf(send, getState);
  };

  let wrapSideEffect = (userSideEffect, ourSelf) => {
    let userSelf = getUserSelf(ourSelf);
    userSideEffect(userSelf);
  };

  let emptySideEffect = _self => ();

  let run = (~router=?, program) => {
    let router =
      switch (router) {
      | Some(router) => router
      | None => Router.hash()
      };

    let ourProgram = BasicProgram.make(program.debugName ++ "_Internal");

    let initialRoute = Route.fromLocation(Router.getCurrent(router));
    let initialSideEffect = ref(emptySideEffect);

    let initialState = () => {
      let routeState =
        program.init(
          initialRoute.path,
          initialRoute.search,
          initialRoute.hash,
        );
      switch (routeState) {
      | State(next) => UserState(next)
      | StateWithSideEffects(next, sideEffect) =>
        initialSideEffect := wrapSideEffect(sideEffect);
        UserState(next);
      };
    };

    let reducer = (ourAction, ourState) => {
      let state = getUserState(ourState);

      let handleUserAction = (action, previous) => {
        let userUpdate = program.toRoute(action, previous);

        switch (userUpdate) {
        | Push(next) =>
          Router.pushAtomic(router, program.serializeState(next));
          Update(UserState(next));
        | PushWithSideEffects(next, sideEffect) =>
          Router.pushAtomic(router, program.serializeState(next));
          UpdateWithSideEffects(
            UserState(next),
            wrapSideEffect(sideEffect),
          );
        | Replace(next) =>
          Router.replaceAtomic(router, program.serializeState(next));
          Update(UserState(next));
        | ReplaceWithSideEffects(next, sideEffect) =>
          Router.replaceAtomic(router, program.serializeState(next));
          UpdateWithSideEffects(
            UserState(next),
            wrapSideEffect(sideEffect),
          );
        | Pop =>
          /* TODO: should this be an atomic operation? How? */
          Router.pop(router);
          NoUpdate;
        | PopWithSideEffects(sideEffect) =>
          /* TODO: should this be an atomic operation? How? */
          Router.pop(router);
          SideEffects(wrapSideEffect(sideEffect));
        };
      };

      let handleRouteChange = (routerAction, previous) => {
        let routeState = program.fromRoute(routerAction, previous);
        switch (routeState) {
        | State(next) => Update(UserState(next))
        | StateWithSideEffects(next, sideEffect) =>
          UpdateWithSideEffects(UserState(next), wrapSideEffect(sideEffect))
        };
      };

      switch (ourAction) {
      | UserAction(action) => handleUserAction(action, state)
      | RouteChanged(routerAction) => handleRouteChange(routerAction, state)
      };
    };

    let render = ourSelf => {
      let userSelf = getUserSelf(ourSelf);
      program.render(userSelf);
    };

    let subscriptions = wrappedState => {
      let historySubscription = self => {
        let _unlisten =
          Router.listen(
            routerAction => self.send(RouteChanged(routerAction)),
            router,
          );
        ();
      };

      let userState = getUserState(wrappedState);

      let subs =
        program.subscriptions(userState)->Belt.List.map(wrapSideEffect);

      [historySubscription, initialSideEffect^, ...subs];
    };

    BasicProgram.run({
      ...ourProgram,
      initialState,
      reducer,
      render,
      subscriptions,
    });
  };
};

module ReactProgram = {
  type state = ReasonReact.reactElement;
  type action =
    | Render(ReasonReact.reactElement);

  let component = ReasonReact.reducerComponent("ApplicationContainer");

  let make = (~program, ~router=?, _children) => {
    ...component,
    initialState: () => ReasonReact.null,
    didMount: self =>
      RouterProgram.run(~router?, program(), view =>
        self.send(Render(view))
      ),
    reducer: (action, _state) =>
      switch (action) {
      | Render(view) => ReasonReact.Update(view)
      },
    render: self => <> {self.state} </>,
  };
};

/* Convenience methods */
let routerProgram = RouterProgram.make;
let run = RouterProgram.run;

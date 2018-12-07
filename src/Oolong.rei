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

module Url = Oolong_Internals.Url;
module Route = Oolong_Internals.Route;
module Router = Oolong_Internals.Router;
module RouterAction = Oolong_Internals.RouterAction;

module RouterProgram: {
  type t('action, 'state, 'view) = {
    debugName: string,
    serializeState: 'state => string,
    init: (list(string), string, string) => routeState('action, 'state),
    fromRoute: (RouterAction.t, 'state) => routeState('action, 'state),
    toRoute: ('action, 'state) => routerUpdate('action, 'state),
    render: self('action, 'state) => 'view,
    subscriptions: 'state => list(sideEffect('action, 'state)),
  };

  let make:
    (~serializeState: 'state => string, string) => t('action, 'state, 'view);

  let run:
    (~router: Router.t=?, t('action, 'state, 'view), 'view => unit) => unit;
};

module ReactProgram: {
  type state;
  type action;

  let component:
    ReasonReact.componentSpec(
      ReasonReact.reactElement,
      ReasonReact.stateless,
      ReasonReact.noRetainedProps,
      ReasonReact.noRetainedProps,
      action,
    );

  let make:
    (
      ~program: unit =>
                RouterProgram.t('action, 'state, ReasonReact.reactElement),
      ~router: Router.t=?,
      array(unit)
    ) =>
    ReasonReact.componentSpec(
      ReasonReact.reactElement,
      ReasonReact.reactElement,
      ReasonReact.noRetainedProps,
      ReasonReact.noRetainedProps,
      action,
    );
};

let routerProgram:
  (~serializeState: 'state => string, string) =>
  RouterProgram.t('action, 'state, 'view);

let run:
  (
    ~router: Router.t=?,
    RouterProgram.t('action, 'state, 'view),
    'view => unit
  ) =>
  unit;

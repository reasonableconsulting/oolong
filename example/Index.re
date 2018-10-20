[%%debugger.chrome];

module S = {
  /* Because people hate ReasonReact.string() for some reason */
  let component = ReasonReact.statelessComponent("StringElement");

  let make = children => {
    ...component,
    render: _self => {
      let str = Js.Array.joinWith(" ", children);

      ReasonReact.string(str);
    },
  };
};

type username = string;
type fullname = string;

type userState =
  | NoUser
  | InvalidUser(username)
  | LoggingIn(username)
  | LoggedIn(username, fullname);

type state = {
  counter: int,
  user: userState,
};
type action =
  | Double(int)
  | Increment
  | Decrement
  | Login(username)
  | LoginSuccess(username, fullname)
  | LoginFailure(username)
  | Nothing;

let app = () => {
  let serializeState = state => {
    let user =
      switch (state.user) {
      | NoUser => ""
      | InvalidUser(user) => user
      | LoggingIn(user) => user
      | LoggedIn(user, _fullname) => user
      };
    Oolong.Url.make([string_of_int(state.counter)], "", user);
  };

  let program = Oolong.routerProgram(~serializeState, "CounterExample");

  let double = (_evt, self) => self.Oolong.send(Double(self.state.counter));

  let login = self =>
    switch (self.Oolong.state.user) {
    | NoUser
    | InvalidUser(_)
    | LoggedIn(_) => ()
    | LoggingIn(username) =>
      Js.log2("Doing login for", username);
      let _ =
        Js.Global.setTimeout(
          () =>
            if (username === "phated") {
              self.Oolong.send(LoginSuccess(username, "Blaine"));
            } else {
              self.Oolong.send(LoginFailure(username));
            },
          500,
        );
      ();
    };

  let getUser = hash =>
    switch (hash) {
    | "" => NoUser
    | username => LoggingIn(username)
    };

  {
    ...program,
    init: (path, _search, hash) =>
      switch (path) {
      | [counter] =>
        Oolong.StateWithSideEffects(
          {counter: int_of_string(counter), user: getUser(hash)},
          login,
        )
      | _ => Oolong.State({counter: 0, user: NoUser})
      },
    fromRoute: (routeAction, _state) => {
      Js.log2("fromRoute", routeAction);
      switch (routeAction) {
      | Push([counter], _search, hash)
      | Replace([counter], _search, hash)
      /* Basically everything is a POP in history - which is pretty terrible */
      | Pop([counter], _search, hash) =>
        Oolong.StateWithSideEffects(
          {counter: int_of_string(counter), user: getUser(hash)},
          login,
        )
      | _ => Oolong.State({counter: 0, user: NoUser})
      };
    },
    toRoute: (action, state) => {
      Js.log3("reducer", action, state);
      switch (action) {
      | Double(num) =>
        Js.log("double");
        Oolong.Replace({...state, counter: num * 2});
      | Increment =>
        Js.log("increment");
        Oolong.Push({...state, counter: state.counter + 1});
      | Decrement =>
        Js.log("decrement");
        Oolong.Push({...state, counter: state.counter - 1});
      | Login(username) =>
        Oolong.PushWithSideEffects(
          {...state, user: LoggingIn(username)},
          login,
        )
      | LoginSuccess(username, fullname) =>
        Oolong.Replace({...state, user: LoggedIn(username, fullname)})
      | LoginFailure(username) =>
        Oolong.Replace({...state, user: InvalidUser(username)})
      | Nothing =>
        Js.log("nothing");
        /* To "ignore" something, just Replace with current state
           This is because I want to guide people into doing the right thing */
        Oolong.Replace(state);
      };
    },
    render: self => {
      Js.log2("render", self);
      let userMessage =
        switch (self.state.user) {
        | NoUser => "Not logged in."
        | LoggingIn(username) => "Logging in as: " ++ username
        | LoggedIn(_username, fullname) => "Welcome " ++ fullname ++ "!"
        | InvalidUser(username) => username ++ " is an invalid user"
        };

      <div>
        <S> {string_of_int(self.state.counter)} </S>
        <button onClick={_ => self.send(Increment)}>
          <S> "Increment" </S>
        </button>
        <button onClick={_ => self.send(Decrement)}>
          <S> "Decrement" </S>
        </button>
        <button onClick={self.handle(double)}> <S> "Double" </S> </button>
        <button onClick={_ => self.send(Nothing)}>
          <S> "Do Nothing" </S>
        </button>
        <button onClick={_ => self.send(Login("phated"))}>
          <S> "Login as phated" </S>
        </button>
        <div> <S> userMessage </S> </div>
      </div>;
    },
  };
};

[%raw
  {|
  function() {
    var app = document.createElement("div");
    app.id = "app";
    document.body.appendChild(app);
  }()
|}
];

Oolong.RouterProgram.run(~router=Oolong.Router.hash(), app(), view =>
  ReactDOMRe.renderToElementWithId(view, "app")
);

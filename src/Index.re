Belt.Debug.setupChromeDebugger();

type state = {counter: int};
type action =
  | Double(int)
  | Increment
  | Decrement
  | Nothing;

let app = () => {
  let program = Program.routerProgram("CounterApp");

  let double: Program.self(action, state) => unit =
    self => {
      Js.log("init side effect");
      self.send(Double(self.state.counter));
    };

  {
    ...program,
    fromRoute: (routeAction, route) =>
      switch (routeAction) {
      | Init =>
        switch (route.path) {
        | ["", counter] => Program.Update({counter: int_of_string(counter)})
        | _ => Program.Update({counter: 0})
        }
      | Push
      | Replace
      | Pop =>
        switch (route.path) {
        | ["", counter] =>
          Program.UpdateWithSideEffects(
            {counter: int_of_string(counter)},
            double,
          )
        | _ => Program.NoUpdate
        }
      },
    toRoute: ({previous, next}) => {
      Js.log3("toRoute", previous, next);
      if (previous == next) {
        Program.NoTransition;
      } else {
        Program.Push(
          Route.make(
            ~path=["", string_of_int(next.counter)],
            ~search="",
            ~hash="",
          ),
        );
      };
    },
    update: (action, state) =>
      switch (action) {
      | Double(num) =>
        Js.log("double");
        Program.Update({counter: num * 2});
      | Increment =>
        Js.log("increment");
        Program.UpdateWithSideEffects({counter: state.counter + 1}, double);
      | Decrement =>
        Js.log("decrement");
        Program.Update({counter: state.counter - 1});
      | Nothing =>
        Js.log("nothing");
        Program.NoUpdate;
      },
    view: self =>
      <div>
        (ReasonReact.string(string_of_int(self.state.counter)))
        <button onClick=(_ => self.send(Increment))>
          (ReasonReact.string("Increment"))
        </button>
        <button onClick=(_ => self.send(Decrement))>
          (ReasonReact.string("Decrement"))
        </button>
        <button onClick=(_ => self.send(Nothing))>
          (ReasonReact.string("Do Nothing"))
        </button>
      </div>,
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

Program.startup(~router=Router.hash(), app(), view =>
  ReactDOMRe.renderToElementWithId(view, "app")
);

type state = {counter: int};
type action =
  | Double(int)
  | Increment
  | Decrement;

let serializeState = state =>
  /* We expose a really simple URL creation method - feel free to generate your string URL in any way */
  Oolong.Url.make([string_of_int(state.counter)], "", "");

let program = Oolong.routerProgram(~serializeState, "CounterExample");

let counterProgram = {
  ...program,
  init: (path, _search, _hash) =>
    switch (path) {
    | [counter] => Oolong.State({counter: int_of_string(counter)})
    | _ => Oolong.State({counter: 0})
    },
  fromRoute: (routeAction, _state) =>
    switch (routeAction) {
    | Push([counter], _search, _hash)
    | Replace([counter], _search, _hash)
    | Pop([counter], _search, _hash) =>
      Oolong.State({counter: int_of_string(counter)})
    | _ => Oolong.State({counter: 0})
    },
  toRoute: (action, state) =>
    switch (action) {
    | Double(num) =>
      /* This is using Replace just as an example - Push might be more appropriate */
      Oolong.Replace({counter: num * 2})
    | Increment => Oolong.Push({counter: state.counter + 1})
    | Decrement => Oolong.Push({counter: state.counter - 1})
    },
  render: self =>
    <div>
      {ReasonReact.string(string_of_int(self.state.counter))}
      <button onClick={_ => self.send(Increment)}>
        {ReasonReact.string("Increment")}
      </button>
      <button onClick={_ => self.send(Decrement)}>
        {ReasonReact.string("Decrement")}
      </button>
      <button onClick={_ => self.send(Double(self.state.counter))}>
        {ReasonReact.string("Double")}
      </button>
    </div>,
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

Oolong.run(~router=Oolong.Router.hash(), counterProgram, view =>
  ReactDOMRe.renderToElementWithId(view, "app")
);

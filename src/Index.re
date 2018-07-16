Belt.Debug.setupChromeDebugger();

type state = {counter: int};

let program = Program.routerProgram("CounterApp");

let app = () => {
  ...program,
  fromRoute: (routeAction, route) => {
    Js.log(routeAction);
    switch (routeAction) {
    | Init =>
      Js.log(route.path);
      switch (route.path) {
      | ["", counter] => Program.Update({counter: int_of_string(counter)})
      | _ => Program.Update({counter: 0})
      };
    | _ => Program.NoUpdate
    };
  },
  /* toRoute: ({previous, next}) => {
       Js.log2(previous, next);
       if (previous == next) {
         Program.NoTransition;
       } else {
         Program.Push(
           Program.makeRoute(
             ~path=["", string_of_int(next.counter)],
             ~search="",
             ~hash="",
           ),
         );
       };
     }, */
  update: state => Program.Update({counter: state.counter + 1}),
  view: self =>
    <div>
      (ReasonReact.string(string_of_int(self.state.counter)))
      <button onClick=(_ => self.send())>
        (ReasonReact.string("Increment"))
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

Program.startup(app(), view => ReactDOMRe.renderToElementWithId(view, "app"));

Js.log("started");

/* BsHistory.push("huh", Program.router); */

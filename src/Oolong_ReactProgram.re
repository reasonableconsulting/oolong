type state = ReasonReact.reactElement;
type action =
  | Render(ReasonReact.reactElement);

let component = ReasonReact.reducerComponent("ApplicationContainer");

let make = (~program, _children) => {
  ...component,
  initialState: () => ReasonReact.null,
  didMount: self =>
    Oolong_Program.startup(program(), view => self.send(Render(view))),
  reducer: (action, _state) =>
    switch (action) {
    | Render(view) => ReasonReact.Update(view)
    },
  render: self => self.state,
};

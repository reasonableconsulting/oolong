module Route = {
  type t = {
    path: list(string),
    search: string,
    hash: string,
  };

  let path = location => {
    let pathname = BsHistory.Location.pathname(location);
    let segments = Js.String.split("/", pathname);

    segments->Belt.Array.keep(segment => segment !== "")->Belt.List.fromArray;
  };

  let search = location => {
    let raw = BsHistory.Location.search(location);
    raw |> Js.String.sliceToEnd(~from=1);
  };
  let hash = location => {
    let raw = BsHistory.Location.hash(location);
    raw |> Js.String.sliceToEnd(~from=1);
  };

  let make = (~path, ~search, ~hash) => {path, search, hash};

  let fromLocation = location =>
    make(
      ~path=path(location),
      ~search=search(location),
      ~hash=hash(location),
    );

  /* TODO: Handle making a url better */
  let toString = route => {
    let search = route.search !== "" ? "?" ++ route.search : "";
    let hash = route.hash !== "" ? "#" ++ route.hash : "";
    "/" ++ String.concat("/", route.path) ++ search ++ hash;
  };
};

module RouterAction = {
  type t =
    | Push(list(string), string, string)
    | Pop(list(string), string, string)
    | Replace(list(string), string, string);

  let make = (action, route) =>
    switch (action) {
    | `Push => Push(route.Route.path, route.Route.search, route.Route.hash)
    | `Pop => Pop(route.Route.path, route.Route.search, route.Route.hash)
    | `Replace =>
      Replace(route.Route.path, route.Route.search, route.Route.hash)
    };
};

module Router = {
  type t = BsHistory.t;

  let atomic = ref(false);
  let isAtomic = () => atomic^;

  let wrapCallback = (callback, location, action) =>
    if (isAtomic() === false) {
      let route = Route.fromLocation(location);
      let routerAction = RouterAction.make(action, route);
      let () = callback(routerAction);
      ();
    };

  let listen = (callback, router) =>
    BsHistory.listen(wrapCallback(callback), router);

  let hash: unit => t =
    () => {
      let historyOpts = BsHistory.makeHashHistoryOptions(~basename="");
      BsHistory.createHashHistory(historyOpts);
    };

  let memory: unit => t =
    () => {
      let historyOpts =
        BsHistory.makeMemoryHistoryOptions(
          ~initialEntries=[|"/"|],
          ~initialIndex=0,
          ~keyLength=6,
        );
      BsHistory.createMemoryHistory(historyOpts);
    };

  let getCurrent = router => BsHistory.location(router);

  [@bs.send] external push: (t, string) => unit = "";
  let pushAtomic = (router, url) => {
    atomic := true;
    push(router, url);
    atomic := false;
  };
  [@bs.send] external replace: (t, string) => unit = "";
  let replaceAtomic = (router, url) => {
    atomic := true;
    replace(router, url);
    atomic := false;
  };
  [@bs.send] external pop: t => unit = "goBack";
  let popAtomic = router => {
    atomic := true;
    pop(router);
    atomic := false;
  };
  [@bs.send] external go: (t, int) => unit = "";
  let goAtomic = (router, amount) => {
    atomic := true;
    go(router, amount);
    atomic := false;
  };
  [@bs.send] external canGo: (t, int) => bool = "";
};

module Url = {
  let make = (path, search, hash) =>
    Route.make(~path, ~search, ~hash)->Route.toString;
};

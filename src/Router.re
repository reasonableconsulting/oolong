type t = BsHistory.t;

module Action = {
  type t =
    | Init
    | Push
    | Pop
    | Replace;

  let fromHistoryAction = action =>
    switch (action) {
    | `Push => Push
    | `Pop => Pop
    | `Replace => Replace
    };
};

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

let listen = (callback, router) =>
  BsHistory.listen(
    (location, action) => {
      let ourAction = Action.fromHistoryAction(action);
      callback(location, ourAction);
    },
    router,
  );

let getCurrent = router => BsHistory.location(router);

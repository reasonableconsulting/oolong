type t = BsHistory.t;

let hash: unit => t =
  () => {
    let historyOpts =
      BsHistory.makeHashHistoryOptions(
        ~basename="",
        /* ~initialEntries=[|"/"|],
           ~initialIndex=0,
           ~keyLength=6, */
      );
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

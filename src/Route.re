type t = {
  path: list(string),
  hash: string,
  search: string,
};

let path = location => {
  let pathname = BsHistory.Location.pathname(location);
  let raw =
    switch (Js.String.get(pathname, Js.String.length(pathname) - 1)) {
    | "/" => Js.String.slice(~from=0, ~to_=-1, pathname)
    | _ => pathname
    };

  raw |> Js.String.split("/") |> Belt.List.fromArray;
};

let search = location => {
  let raw = BsHistory.Location.search(location);
  raw |> Js.String.sliceToEnd(~from=1);
};
let hash = location => {
  let raw = BsHistory.Location.hash(location);
  raw |> Js.String.sliceToEnd(~from=1);
};

let make = (~path, ~hash, ~search) => {path, hash, search};

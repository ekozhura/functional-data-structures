module type Ordered = {type t; let eq: (t, t) => bool; let lt: (t, t) => bool; let leq: (t, t) => bool;};

module IntElement: Ordered with type t = int = {
  type t = int;
  let eq = (a, b) => a === b;
  let lt = (a, b) => a < b;
  let leq = (a, b) => a <= b;
};

module type Set = {
  type elem;
  type set;

  let empty: set;
  let insert: (elem, set) => set;
  let member: (elem, set) => bool;
};

module UnbalancedSet = (Element: Ordered) : (Set with type elem = Element.t) => {
  type elem = Element.t;
  type tree =
    | E
    | T(tree, elem, tree);
  type set = tree;

  let empty = E;

  let rec member = (x, set) =>
    switch (set) {
    | E => false
    | T(a, y, b) =>
      if (Element.lt(x, y)) {
        member(x, a);
      } else if (Element.lt(y, x)) {
        member(x, b);
      } else {
        true;
      }
    };

  let rec insert = (x, set) =>
    switch (set) {
    | E => T(E, x, E)
    | T(a, y, b) =>
      if (Element.lt(x, y)) {
        T(insert(x, a), y, b);
      } else if (Element.lt(y, x)) {
        T(a, y, insert(x, b));
      } else {
        set;
      }
    };
};

module TreeOfInt = UnbalancedSet(IntElement);

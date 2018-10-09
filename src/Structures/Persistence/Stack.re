module type Stack = {
  type stack('a);

  let empty: stack('a);
  let isEmpty: stack('a) => bool;
  let cons: ('a, stack('a)) => stack('a);
  let head: stack('a) => 'a;
  let tail: stack('a) => stack('a);
  let concat: (stack('a), stack('a)) => stack('a);
  let update: (stack('a), int, 'a) => stack('a);
};

exception EmptyList;
exception Subscript;

module List: Stack = {
  type stack('a) = list('a);
  let empty = [];
  let isEmpty =
    fun
    | [_, ..._] => false
    | _ => true;

  let cons = (x, s) => [x, ...s];
  let head =
    fun
    | [x, ..._] => x
    | [] => raise(EmptyList);

  let tail =
    fun
    | [_, ...q] => q
    | [] => raise(EmptyList);

  let rec concat = (xs, ys) =>
    switch (xs) {
    | [] => ys
    | [x, ...xs] => [x, ...concat(xs, ys)]
    };

  let rec update = (ls, idx, value) =>
    switch (ls) {
    | [] => raise(Subscript)
    | [x, ...xs] =>
      switch (idx) {
      | 0 => [value, ...xs]
      | _ => [x, ...update(xs, idx - 1, value)]
      }
    };
};

module CustomStack: Stack = {
  type stack('a) =
    | Nil
    | Cons('a, stack('a));

  let empty = Nil;
  let isEmpty =
    fun
    | Nil => true
    | _ => false;

  let cons = (x, s) => Cons(x, s);

  let head =
    fun
    | Nil => raise(EmptyList)
    | Cons(x, _) => x;

  let tail =
    fun
    | Nil => raise(EmptyList)
    | Cons(_, s) => s;

  let rec concat = (xs, ys) =>
    if (isEmpty(xs)) {
      ys;
    } else {
      cons(head(xs), concat(tail(xs), ys));
    };

  let rec update = (ls, idx, value) =>
    if (isEmpty(ls)) {
      raise(Subscript);
    } else {
      switch (idx) {
      | 0 => cons(value, tail(ls))
      | _ => cons(head(ls), update(tail(ls), idx - 1, value))
      };
    };
};

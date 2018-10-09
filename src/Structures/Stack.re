/**
 * Based on chapter 2 "Persistence"
*/
module type Stack = {
  type stack('a);

  let empty: stack('a);
  let isEmpty: stack('a) => bool;
  let cons: ('a, stack('a)) => stack('a);
  let head: stack('a) => 'a;
  let tail: stack('a) => stack('a);
};

exception EmptyList;

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
};

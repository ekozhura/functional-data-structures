open Stack;
open Set;

let myList = List.cons("a", List.empty);
let myCustomList = CustomStack.cons("a", CustomStack.empty);

let concatedList = List.concat(myList, List.cons("b", List.empty));
let concatedStack = CustomStack.concat(myCustomList, CustomStack.cons("b", CustomStack.empty));

Js.log(myList);
Js.log(myCustomList);

Js.log(List.update(concatedList, 1, "c"));
Js.log(CustomStack.update(concatedStack, 1, "c"));

let myTree = TreeOfInt.insert(4, TreeOfInt.insert(2, TreeOfInt.insert(3, TreeOfInt.insert(1, TreeOfInt.empty))));
Js.log(myTree);

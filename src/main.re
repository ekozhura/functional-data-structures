open Stack;

let myList = List.cons("a", List.empty);
let myCustomList = CustomStack.cons("a", CustomStack.empty);

Js.log(myList);
Js.log(myCustomList);

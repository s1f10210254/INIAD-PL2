exception EmptyStack

type 'a stack = { mutable s: 'a list };;

let new_stack () = { s = [] };;

let push stk x =
  stk.s <- x :: stk.s
;;
let pop stk =
  match stk.s with
  | hd :: tl ->
    stk.s <- tl;
    hd
  | [] -> raise EmptyStack
  ;;
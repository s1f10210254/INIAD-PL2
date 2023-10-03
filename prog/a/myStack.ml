exception EmptyStack

type 'a t = { mutable s: 'a list };;

let create () = { s = [] };;

let push x stk =
  stk.s <- x :: stk.s
;;

let pop stk =
  match stk.s with
  | hd :: tl ->
    stk.s <- tl;
    hd
  | [] -> raise EmptyStack
;;

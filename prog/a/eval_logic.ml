type formula =
  And of formula * formula
  | Or of formula * formula
  | Not of formula
  | True
  | False
;;

let rec eval_logic formula =
  match formula with
  | And (f1, f2) -> eval_logic f1 && eval_logic f2
  | Or (f1, f2) -> eval_logic f1 || eval_logic f2
  | Not f -> not (eval_logic f)
  | True -> true
  | False -> false
;;

eval_logic (And(True, Or(True, Not(False))));;
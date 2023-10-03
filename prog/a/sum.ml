let rec sum lst =
  match lst with
    first::rest -> first +. sum rest
  | [] -> 0.0
;;

(*
let sum lst =
  let rec sum_iter result lst =
    match lst with
      first::rest -> sum_iter (result +.first) rest
    | [] -> result
  in sum_iter 0.0 lst
;;
*)

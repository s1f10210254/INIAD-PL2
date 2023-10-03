(*リストの長さを返す関数を以下の二通りで実装せよ*)
(*1,末尾再帰による実装*)
(*
let length lst =
  let rec length_aux acc = function
    | [] -> acc
    | _::t -> length_aux (acc+1) t 
in length_aux 0 lst;;

length [1; 2; 3; 4];;
length["ken"; "sakamura"];;
length [];;
*)

(*List.fold_leftを用いた実装*)
let length lst =
  List.fold_left (fun acc _ -> acc + 1) 0 lst;;


length [1; 2; 3; 4];;
length["ken"; "sakamura"];;
length [];;

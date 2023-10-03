(*List.filterと同じ計算を行う関数filterを実装せよ*)
let rec filter f xs =
  match xs with
  | [] -> []
  | x :: xs' -> if f x then x :: filter f xs' else filter f xs';;

filter(fun x -> x mod 2 = 0)[1;2;3;4;5];;


(*
let rec filter f xs =
  match xs with
  | [] -> []
  | hd :: tl ->
     if f hd then 
      hd :: filter f tl else 
        filter f xs;;
*)

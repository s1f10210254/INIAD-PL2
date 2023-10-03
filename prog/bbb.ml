(*浮動小数のリスト xs を引数にとり、
その中に含まれる数値の正負を反転させたリストを返す関数
 inv : float list -> float list を定義せよ*)
(* let rec inv xs =
  match xs with
    [] -> []
    | hd::tl -> hd * -1 ;; *)

(* let inv xs =
  List.map (fun x -> "-" ^ x) xs;; *)

let rec inv xs =
  match xs with
    first::rest -> (first * -1)::(inv rest)
  | [] -> [];;

inv [1; -2; -3; 4; -5];;



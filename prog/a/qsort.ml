(*整数のリストを昇順にクイックソートして得られるリストを
  返す関数qsort : int list -> int listを定義せよ
   *)

let rec qsort = function
  | [] -> []
  | pivot :: rest ->
    let is_less x = x < pivot in
    let left, right = List.partition is_less rest in
    qsort left @ (pivot :: qsort right);;

qsort[3; 1; 4; 2; 7; 9; 8; 5; 6];;

qsort[1; 8; 4; 2; 3; 9; 6; 7; 5];;
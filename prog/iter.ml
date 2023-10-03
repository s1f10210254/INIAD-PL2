(* 以下の関数を末尾再帰となるように書き換えよ *)

(* let rec remove_zero numbers = 
  match numbers with
    0::tl -> remove_zero tl
  | hd::tl -> hd::(remove_zero tl)
  | [] -> [] *)

let remove_zero numbers =
  (*補助関数を使う。蓄積用の引数を追加*)
  let rec remove_zero_iter result nums =
    (*再帰呼び出しの結果をそのまま返す*) 
    match nums with
      0::tl -> remove_zero_iter result tl
    | hd::tl -> remove_zero_iter (hd::result) tl
    | [] -> result
    (*終了条件を待つ*)
  in List.rev (remove_zero_iter [] numbers)

let rec pow a n =
  if n = 0 then 1
  else a * pow a (n-1);;
pow 3 4;;

let rec pow x y =
  let rec pow_iter result y =
    if y <= 0 then result
    else
    let result' = x * result in
    pow_iter result' (y-1)
  in pow_iter 1 y;;
pow 3 4;;
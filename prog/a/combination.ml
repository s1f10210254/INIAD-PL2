(*
すべての組み合わせ「オプショナル」
リストxsと整数nを受け取り、xsから要素をn個取り出す
すべての組み合わせをリストにして返す関数を定義せよ
*)

let rec combination xs n acc =
  if n = 0 then [acc]
  else match xs with
       | [] -> []
       | hd::tl -> 
         (combination tl (n-1) (hd::acc))
         @ (combination tl n acc);;

let combination xs n = combination xs n [];;

combination [1; 2; 3] 2;;
combination [1; 2; 3] 0;;
combination [] 3;;
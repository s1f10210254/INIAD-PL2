(*
正の整数 n を受け取り、10進数におけるnの桁数を返す関数digit : int -> int を定義せよ
可能であれば末尾再帰にしてみよう   
*)

let digit n =
  let rec digit_helper n acc =
    if n < 10 then acc + 1
    else digit_helper (n / 10) (acc + 1)
  in
  digit_helper n 0;;

digit 1000;;
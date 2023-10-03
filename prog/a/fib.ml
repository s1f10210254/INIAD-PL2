(*
フィボナッチ数Fnは以下のように定義される
F0 = 0
F1 = 1
Fn = Fn-1 + Fn-2 (n >= 2)
１，この定義に従って関数fib1 : int -> を定義せよ
２、さらに末尾再帰によりO(n)でフィボナッチ数の計算を行う
関数fib2 : int -> int を定義せよ
*)

let rec fib1 n =
  match n with
  | 0 -> 0
  | 1 -> 1
  | _ -> fib1 (n-1) + fib1 (n-2);;

let fib2 n =
  let rec fib_helper a b i =
    if i = n then a
    else fib_helper b (a+b) (i+1)
  in
  match n with
  | 0 -> 0
  | 1 -> 1
  | _ -> fib_helper 0 1 2;;

fib1(40);;
fib2(40);;
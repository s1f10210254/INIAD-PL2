(*
導関数を求める関数
関数f: float ->floatを引数に与えると、その導関数f'を返す
関数derivを定義せよ   
厳密解を求めることは一般にはできないので近似値でよい
*)

let deriv f =
  let delta = 0.0001 in
  fun x -> (f (x +. delta) -. f (x -. delta)) /. (2.0 *. delta);;


let f x = x ** 2.0;;
let f' = deriv f;;
f' 10.0;;
(*構文木として表現された数式を計算（実行）し、
計算結果を返す関数eval : expression -> をOcamlで定義せよ*)

type expression =
  Binop of char * expression * expression
| Number of int;;

let rec eval expr =
  match expr with
  Binop('*',lhs,rhs)->eval lhs * eval rhs
  | Binop('/',lhs,rhs) -> eval lhs / eval rhs
  | Binop('+', lhs, rhs) -> eval lhs + eval rhs
  | Binop('-', lhs, rhs) -> eval lhs - eval rhs
  | Number(x) -> x
  | _ -> failwith "Unsupported operation"
;;

eval (Binop('+', Number(1),Binop('*',Number(2),Number(3))));;

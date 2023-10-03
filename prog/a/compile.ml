type instruction = Add | Sub | Mul | Div | Push of int;;

type expression =
  Binop of char * expression * expression
| Number of int;;


let rec compile (exp : expression) : instruction list =
  match exp with
  | Number n -> [Push n]
  | Binop ('+', e1, e2) -> compile e1 @ compile e2 @ [Add]
  | Binop ('-', e1, e2) -> compile e1 @ compile e2 @ [Sub]
  | Binop ('*', e1, e2) -> compile e1 @ compile e2 @ [Mul]
  | Binop ('/', e1, e2) -> compile e1 @ compile e2 @ [Div]

let example = compile (Binop ('+', Number (1), Binop ('*', Number (2), Number (3)))) ;;
  (*演習課題６とおおよそ同じ構造だがリストを返utすので
     左側のンパイル結果と右側のコンパイル結果と[operationの記号
     に対応した命令]を結合する*)
  (*NumberのときはPush(int)*)
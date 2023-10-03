(* 二分探索木の定義 *)
type 'a btree = Leaf | Node of 'a * 'a btree * 'a btree;;
(* 要素を追加する関数 *)
let rec append_bst x t =
  match t with
    Node (v, left, right) ->
      if x < v then Node (v, append_bst x left, right)
      else Node (v, left, append_bst x right)
    | Leaf -> Node (x, Leaf, Leaf)
;;

append_bst 5 (Node(8, Node(3, Node(1, Leaf, Leaf), Leaf), Node(10, Leaf, Leaf)));;
type 'a btree = Leaf | Node of 'a * 'a btree * 'a btree;;

let rec min_bst t =
  match t with
  Node(v, Leaf, _) -> Some v
  | Node(_, left, _) -> min_bst left
  | Leaf -> None
;;
min_bst(Node(8, Node(3, Node(1, Leaf, Leaf), Leaf), Node(10, Leaf, Leaf)));;
min_bst Leaf;;
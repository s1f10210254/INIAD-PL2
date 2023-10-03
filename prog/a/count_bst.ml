type 'a btree = Leaf | Node of 'a * 'a btree * 'a btree;;

let rec count_bst t =
  match t with
  Node(_, left, right) -> 1 + count_bst left + count_bst right
  | Leaf -> 0
;;
count_bst(Node(8,Node(3, Node(1, Leaf, Leaf),Leaf),Node(10,Leaf,Leaf)));;

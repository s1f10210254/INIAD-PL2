let add x y z = x + y + z;;
add 1 2 3;;

(*整数と浮動小数を一つずつ引数にとり、その和を返す関数add_floatを定義する*)
let add_float n x =
  float_of_int n +. x;; 
add_float 1 2.3;;



(*整数ｘと浮動小数yを受け取り、そのうち大きいほうの値を浮動小数として返す関数largerを定義せよ*)
let larger x y =
  let x = float_of_int  x in if x > y then x else y;;
(*最大公約数*)
let rec gcd a b =
  if b = 0 then a
  else gcd b(a mod b);;

gcd 24 16;;

(*偶然と奇数の多数決*)
let even_vs_odd a b c d e =
  let count_even = (if a mod 2 = 0 then 1 else 0) +
    (if b mod 2 = 0 then 1 else 0) +
    (if c mod 2 = 0 then 1 else 0) +
    (if d mod 2 = 0 then 1 else 0) +
    (if e mod 2 = 0 then 1 else 0) in
  let count_odd = 5 -count_even in
  if count_even > count_odd then true else false;;

even_vs_odd 1 2 3 4 5;;

(*文字列ｓの内容をn回繰り返してできる文字列を返す関数*)
let rec mult n s =
  if n <= 0 then ""
  else s ^ mult (n-1) s

(*累乗の計算*)
let rec pow a n =
  if n = 0 then 1
  else a * pow a (n-1);;

pow 3 4;;

(*素数判定を行う*)
let isprime n =
  let rec check i =
    if n = i then true
    else if n mod i = 0 then false
    else check (i + 1)
  in
  check 2;;

isprime 3;;
isprime 6;;

(*導関数を求める関数*)
let deriv f =
  let delta = 0.0001 in
  fun x -> (f (x +. delta) -. f(x -. delta)) /. (2.0 *. delta);;

let f x = x ** 2.0;;
let f' = deriv f;;
f' 10.0;;

(*リストの長さ*)

(*末尾再帰による実装*)
let rec length lst =
  match lst with
  _::tl -> 1 + length tl
  | [] -> 0;;

length [1;2;3;4];;

(*List.fold_leftを用いる*)
let length_fold lst =
  List.fold_left(fun count _ -> count + 1) 0 lst;;
length_fold[1;2;3;4];;

(*高階関数ー＞List.filterと同じ計算を行う関数filterを実装せよ*)
let rec filter f xs =
  match xs with
  [] ->[]
  | hd::td -> if f hd then hd::filter f td
    else filter f td;;

(*フィボナッチ数の計算*)
let rec fib1 n =
  match n with
  0 -> 0
  | 1 -> 1
  |_ ->fib1(n-1) + fib1(n-2);;
fib1 10;;
(*末尾再帰によるフィボナッチ数の計算*)
(*fib_iter -> a 次の項、　b  現在の項、　p まだ進めるべき項の数*)
let fib2 n =
  let rec fib_iter a b p =
    if p = 0 then b else fib_iter (a + b) a (p-1)
  in
  fib_iter 1 0 n;;

fib2 10;;

(* リストの分解 リストの合計*)
let rec sum xs =
  match xs with
    hd::tl -> hd + (sum tl)
  | [] -> 0;;
sum [1;2;3;4;5];;

(* リストの結合 *)
let rec (@) lst1 lst2 =
  match lst1 with
   [] -> lst2
  | hd::tl -> hd::(tl @ lst2);;

(*３要素のリストの合計を永さんする関数*)
let sum_list xs =
  match xs with
    [x1; x2; x3] -> x1 + x2 + x3
  | _ -> failwith "error";;

(*浮動小数のリストを引数にとり、その合計を返す関数*)
let rec sumfloat lst =
  match lst with
  | [] -> 0.0
  hd::tl -> hd +. sum tl;;
(*文字列のリストnamesを引数にとり、その中のすべてに-sanをつける*)
let addsan names =
  List.map (fun name -> name ^ "-san") names;;
let rec addsan names =
  match names with
    first::rest -> (first ^ "-san")::(addsan rest)
  | [] -> [];;
(*引数に与えられたリストxsから先頭のn個分の要素からなるリストを返す関数*)
let rec limit xs n =
  match xs with
    hd::tl -> if n <= 0 then []
   else hd::(limit tl (n-1))
  | [] -> [];;
limit [1;2;3;4;5] 3;;

(* リストを逆順にして返す関数 *)
let rev xs =
  let rec rev_iter result xs = 
    match xs with
      hd::tl -> rev_iter (hd::result) tl
    | [] -> result
  in rev_iter [] xs;;
rev[1;2;3];;

(*リストの中の文字列の頭文字をつなげる*)
let rec abbrev xs = 
  match xs with
    hd::tl ->
      (String.sub hd 0 1) ^ (abbrev tl)
  | [] -> "";;
abbrev["British"; "Broadcasting"; "Corp"];;

(*fold:各要素に対する処理を関数ｆとして受け取り、その内容を繰り返す関数*)
let rec fold f x xs =
  match xs with
    hd::tl -> fold f (f x hd) tl
  | [] -> x;;

let sum = fold (+) 0;;
sum [1;2;3];;

let abbrev = fold (fun result x -> result
  ^ (String.sub x 0 1)) "";;
abbrev["British"; "Broadcasting"; "Corp"];;


(*単方向リンクリスト　リストの長さを求める関数*)

type  int_list = Node of int * int_list | Null;;
let rec len xs = 
  match xs with
    Node(_,tl) -> 1 + len tl
  | Null -> 0;;

let lst = Node(1, Node(2, Node(3, Null))) in len lst;;

(*多層単方向リスト型('a list に相当)*)
type 'a llist = Node of 'a * 'a llist | Null;;

let numbers = Node(1, Node(2, Node(3, Null)));;
let strings = Node("Hello", Node("world", Null));;

(*option型　存在しない・無効かもしれない値を表現できる*)
(*リストの先頭を返す関数*)
let hd_opt xs = match xs with
    x::_ -> Some(x)
  | [] -> None;;

hd_opt[1;2;3];;
hd_opt [];;

(*整数の割り算X/Yを計算する関数div　有効な値が得られないかもしれない処理に利用*)
let div x y =
  if y <> 0 then Some(x / y) else None;;
div 10 3;;
div 7 0;;

(*二分木の表現*)
type 'a btree =
    Node of 'a * 'a btree * 'a btree
  | Leaf;;

(*二分木の表現と操作例*)
let t =
  Node(1,
    Node(2,
      Node(4, Leaf,
        Node(5, Leaf, Leaf)),
      Leaf),
    Node(7, Node(6, Leaf,Leaf),
      Node(8, Leaf,
        Node(9, Leaf, Leaf))));;
let Node(v, t1, t2) = t
  in Node(v,t2,t1);;
(*二分木の深さ（高さ）*)
let rec depth t = 
  match t with
    Node(_,t1,t2) ->
      1 + max (depth t1) (depth t2)
    | Leaf -> 0;;


(*二分木tと値eを引数にとりtの中にeが含まれるかどうか真偽値として返す*)

let rec contains t e =
  match t with
    Node(v,t1,t2)->
      if v = e then true 
      else if v > e then contains t1 e
      else contains t2 e
   | Leaf -> false;;

let tree = Node(1, Node(3, Leaf, Node(6, Leaf, Leaf)), Node(4, Leaf, Node(2, Leaf, Leaf)));;

contains tree 5;;



(*幅優先探索を実装*)
let bfs t =
  let rec bfs_iter result nodes =
    if nodes = [] then result else
    let (newresult, nodes') = 
      List.fold_left (fun (rs, ns) n ->
        match n with
          Node(v, t1, t2) -> (rs @ [v], t1::t2::ns)
        | Leaf -> (rs, ns)) ([], []) nodes
      in bfs_iter (result @ newresult) nodes'
    in bfs_iter [] [t];;

(*二分木の要素の数え上げ*)
let rec count_bst tree =
  match tree with
   Leaf -> 0;
  | Node(_, left, right) -> 1 + count_bst left + count_bst right;;

count_bst(Node(8,Node(3,Node(1,Leaf,Leaf),Leaf),Node(10,Leaf,Leaf)));;

(*二分木探索の最小値*)
let rec min_bst tree =
  match tree with
  Node(v, Leaf, _) -> Some v
  | Node(_, l, _) -> min_bst l
  | Leaf -> None;;
min_bst(Node(8,Node(3,Node(1,Leaf,Leaf),Leaf),Node(10,Leaf,Leaf)));;

(*二分木探索木への要素の追加*)
let rec append_bst x tree =
  match tree with
  Node(v,t1,t2) ->
    if x > v then Node(v, t1, append_bst x t2)
    else if x < v then Node(v,append_bst x t1, t2)
    else Node(v, t1, t2)
  | Leaf -> Node(x, Leaf, Leaf);;
append_bst 5 (Node(8,Node(3,Node(1,Leaf,Leaf),Leaf),Node(10,Leaf,Leaf)));;

(*インタプリタの実装*)
type expression = 
    Binop of char * expression * expression
  | Number of int;;

let rec eval expr =
  match expr with
  Binop('+', left, right) -> eval left + eval right
  | Binop('-', left, right) -> eval left - eval right
  | Binop('*', left, right) -> eval left * eval right
  | Binop('/', left, right) -> eval left / eval right
  | Binop(_,_,_) -> failwith "error"
  | Number(x) -> x;;

eval(Binop('+', Number(1), Binop('*', Number(2),Number(3))));;

(*コンパイラの実装*)
type instruction = 
    Add | Sub | Mul | Div
  | Push of int;;

let rec compile expr = 
  match expr with
  Binop('+', left, right) -> compile left @ compile right @ [Add]
  | Binop('-', left, right) -> compile left @ compile right @ [Sub]
  | Binop('*', left, right) -> compile left @ compile right @ [Mul]
  | Binop('/', left, right) -> compile left @ compile right @ [Div]
  | Binop(_,_,_) -> failwith "error"
  | Number(x) -> [Push(x)];;
compile(Binop('+', Number(1), Binop('*', Number(2),Number(3))));;

(* スタックの実装
type 'a stack = { mutable s: 'a list};;

let new_stack () =
  { s = []};;
  (*空のリスト*)
let push stk x =
  stk.s <- x :: stk.s;
  List.length stk.s;;
  (*要素を先頭にくっつけたリスト*)
let pop stk =
  match stk.s with
  [] -> failwith "Stack is empty"
  | head :: tail ->
    stk.s <- tail;
    head;;
    (*先頭要素と残りに分けて、残りを代入してから;
      要素を返す*)
let s = new_stack();;
push s 1;;
push s 2;;
pop s;;
pop s;;
pop s;; *)


(*リストxsと正整数nを引数にとり、xsのn番目の要素を取り除いたリストを返す関数*)
let rec remove xs n =
  match(xs, n) with
    ([], _) ->[]
  | (_::tl , 0) ->tl
  | (hd :: tl,_) -> hd :: (remove tl (n-1));;
remove [1;2;3;4;5;6] 3;;
remove [1;2;3;4;5;6] 0;;


(*関数fと正整数nを受け取り、fをn回合成した関数を返す関数applyを定義せよ*)
let rec apply f n x =
  if n <= 0 then x
  else apply f (n-1) (f x);;
(*この高階関数applyを用いて√aの近似値を計算する関数*)
let mysqrt a =
  let f x = x ** 2.00 -. a in
  let f' x = 2.0 *. x in (*fの導関数*)
  let g x = x -. (f x) /. (f' x) in
  (apply g 10) 1.0;; (*初期値１で１０回反復*)
mysqrt 2.0;;
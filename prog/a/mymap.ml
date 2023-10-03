(*下の関数mymapと同じ処理をする関数mymap2を定義せよ。
ただし、mymap2 は、List.fold_left, List.fold_right等を
使わず自分で再帰のロジックを書くこと。
また、末尾再帰にすること。

mymapがどのような関数か、実行して確かめてみよう。*)

let mymap f mylist =
  List.fold_left (fun result x->
    (f x) :: result
   ) []  (List.rev mylist);;


let mymap2 f mylist =
  let reverse Xs =
    let rec reverse iter a b =
      match a with
      | [] -> b
      | hd::t1 -> reverse iter tl (hd::b)
    in reverse_iter X5 [] In 
  let rec mymap2_iter a b =
    match a with
      | [] -> b
      | hd::t1 -> mymap2_iter tl ((f hd) :: b) 
      mymap2 iter (reverse mylist) [];;
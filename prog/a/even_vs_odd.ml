(*偶数と奇数の多数決
５つの整数を引数として受け取り、そのうち偶数が奇数より
多い場合はTRUE、そうでない場合はFALSEを返す関数   
*)

let even_vs_odd a b c d e =
  let count_even = (if a mod 2 = 0 then 1 else 0) +
                   (if b mod 2 = 0 then 1 else 0) +
                   (if c mod 2 = 0 then 1 else 0) +
                   (if d mod 2 = 0 then 1 else 0) +
                   (if e mod 2 = 0 then 1 else 0) in
  let count_odd = 5 - count_even in
  count_even > count_odd;;

even_vs_odd 1 2 3 4 5;;
even_vs_odd 2 (-3) 4 5 (-6);;

(*let rec pow a n =
  if n = 0 then 1
  else a * pow a (n - 1);;   
*)
(*末尾再帰となるようにせよ*)
let pow a n =
  let rec pow_helper a n acc =
    if n = 0 then acc
    else pow_helper a (n-1) (acc*a)
  in
  pow_helper a n 1;;

pow 3 4;;

pow 2 10;;
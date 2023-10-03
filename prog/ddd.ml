let rec amax xs =
  match xs with
    hd::tl -> if xs >= 7 then hd::(amax tl)
    else hd;;
amax [1; 3; 2; 7; 4; 6];;
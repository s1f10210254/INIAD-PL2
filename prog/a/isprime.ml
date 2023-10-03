(*素数判定を行う*)
let isprime n =
  let rec check i =
    if i = n then true
    else if n mod i = 0 then false
    else check (i + 1)
  in
  check 2;;

isprime 3;;
isprime 4;;
isprime 5;;
isprime 6;;
let rec sigma n =
  if n <= 0 then 0
  else n + sigma (n-1) + 1;;

  sigma 10;;
  sigma 3;;
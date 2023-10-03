let plist () lst=
  match lst with
  [] -> []
  | hd::tl -> print_endline();;

plist ["I"; "love"; "INIAD"];;
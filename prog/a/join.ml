let rec join words =
  match words with
  first::rest -> first ^ " " ^ join rest
  | [] ->""

;;

join ["I"; "am"; "INIAD"];;
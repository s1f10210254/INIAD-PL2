let print_chars str =
  let len = Strk g.length str in
  let rec iter i =
    if i < len then
      begin
        cdprint_chars str.[i];
        print_newLine ();
        iter (i+1)
      end
    in iter 0;;

let print_chars' str =
    String.iter (fun c -> print_chars(c); print_newLine());;
let print_chars str =
  let len = String.length str in
  let rec iter i =
    if i < len then
      begin
        print_char str.[i];
        print_newline();
        iter (i+1)
      end
  in iter 0;;

let print_chars' str =
  String.iter (fun c -> print_chars(c); print_newLine());;

let () =
  let input = "Hello, World!" in
  print_chars input;
  print_newline ();
  print_chars' input
;;
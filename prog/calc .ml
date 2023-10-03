(* REPL *)

type expression =
    Binop of char * expression * expression
  | Number of int

let parse expr =
  let expr = 
    let r = ref "" in
    String.iter (fun c -> if c <> ' ' then r := !r ^ (String.make 1 c) else ()) expr;
    !r
  in let len = String.length expr in
  let rec parse_addsub pos =
    let rec iter (pos, e) =
      if pos >= len then pos, e
      else match expr.[pos] with
          '+' | '-' as op ->
          let pos, e2 = parse_muldiv (pos+1) in iter (pos, Binop(op, e, e2))
        | _ -> (pos, e)
    in iter (parse_muldiv pos)
  and parse_muldiv pos =
    let rec iter (pos, e) =
      if pos >= len then pos, e
      else match expr.[pos] with
          '*' | '/' as op ->
          let pos, e2 = parse_simple (pos+1) in iter (pos, Binop(op, e, e2))
        | _ -> (pos, e)
    in iter (parse_simple pos)
  and parse_simple pos =
    if pos >= len then failwith "Parse error"
    else match expr.[pos] with
        '(' ->
        let pos, head = parse_addsub (pos+1) in
        if pos >= len || expr.[pos] <> ')' then failwith "Parse error"
        else (pos+1), head
      | c when c >= '0' && c <= '9' ->
        let char_of_int c = Char.code c - Char.code '0' in
        let rec iter (pos, v) =
          if pos >= len || expr.[pos] < '0' || expr.[pos] > '9' then (pos, v)
          else iter (pos+1, v * 10 + char_of_int expr.[pos])
        in let (pos, v) = iter (pos+1, char_of_int c)
        in pos, Number(v)
      | _ -> failwith "Parse error"
  in let pos, head = parse_addsub 0 in
  if pos < len then failwith "Parse error"
  else head;;

let rec eval expr =
  match expr with
  Binop('+', left, right) -> eval left + eval right
  | Binop('-', left, right) -> eval left - eval right
  | Binop('*', left, right) -> eval left * eval right
  | Binop('/', left, right) -> eval left / eval right
  | Binop(_,_,_) -> failwith "error"
  | Number(x) -> x;;
  
let rec repl () = 
  print_string "Enter an expression to evaluate: ";
  let expr_string = read_line() in
  try
  let expr = parse expr_string in
    let result = eval expr in
    print_endline(string_of_int result);
    repl()
  with
  | Failure msg ->
    print_endline("Error: " ^ msg);
    repl();;
repl();; 
(* 文字列のリストを受け取り、その中に含まれる文字列を順に標準出力に表示する *)
let rec plist words =
  match words with
    hd::words -> print_endline hd ;plist words
  | [] -> ();;

plist ["I"; "love"; "INIAD"];;

(* 浮動小数のリストを引数にとり、数値の政府を反転させたリスト *)
let rec inv xs =
  match xs with
    first::rest -> (first *. -1.0)::(inv rest)
  | [] -> [];;

inv [1.0; -2.0; -3.0; 4.0; -5.0];;


(* type expression =
    Add of expression * expression    (* e1 + e2 *)
  | Sub of expression * expression    (* e1 - e2 *)
  | UMinus of expression              (* -e *)
  | Value of int *)

(* let rec eval expr =
  match expr with
    Add (e1,e2) -> (eval e1) + (eval e2)
  | Sub (e1,e2) -> (eval e1) - (eval e2)
  | Uminus(e) -> (eval e) * (-1)
  | Value n -> n;; *)

(* 整数のリストを引数にとり、その整数に０が含まれていればtrueをそうでなければfalse *)
let rec hazzero xs =
  match xs with
    hd::tl -> 
      if hd = 0 then true
      else hazzero tl;
    | [] -> false;;

(* 正の整数nと文字列sを引数にとり、文字列sの内容をn回繰り返してできる文字列を返す返す *)
let rec mult n s =
  if n <= 0 then ""
  else s ^ mult(n-1) s;;

(* リストxsを引数にとり、その先頭要素を除いたリストを返す *)
let rec remove xs =
  match xs with
    _::tl -> tl
    | [] -> [];;
remove[1;2;3];;

(* 2つの参照型の値a,bを受け取り、その中身を入れ替える関数 *)
let exchange a b =
  let aval = !a and bval = !b in
  begin
    a := bval;
    b := aval
  end;;
let a = ref 0;;
let b = ref 1;;
exchange a b;;


(*整数ｘと浮動小数ｙを受け取り、そのうち大きいほうの値浮動小数として返す*)
let larger x y = 
  let x = float_of_int x in
  if x > y then x
  else y;;
larger 3 4.0
  
(*正の整数ｎと任意の値ｓを引数にとり、ｓをリストにｎ個並べたリストを返す*)
let rec duplicate n s =
  if n <= 0 then []
  else s:: duplicate(n-1) s;;
duplicate 3 "iniad";;

(*正整数のリストxsを引数にとり、最大値を返す関数*)
(* let rec amax xs = 
  match xs with
   hd::tl -> max hd (amax tl)
  | [] -> [];;
amax [1; 3; 2; 7; 4; 6];; *)

(*pyexpr型でpythonの式の一部を表現する*)
(* type pyexpr =
 Add of pyexpr * pyexpr (* x + y *)
 | Div of pyexpr * pyexpr (* x // y *)
 | Value of int *)
(* let rec calc e =
  match e with
  Add(lhs,rhs) -> calc lhs + calc rhs
  | Div(lhs, rhs) -> calc lhs / calc rhs
  | Value(v) -> v *)

  (* type pyval =
  PyInt of int      (* Pythonのint型 *)
| PyFloat of float  (* Pythonのfloat型 *)

let rec pyval e =
  match e with
    Add_int(e1,e2) => (eval e1) + (eval e2) 
    | ADD_float(e1,e2) => (eval e1) +. (eval e2)
    | Value (v) -> v *)

(* exception Type_mismatch *)

(* type value = *)
    (* String of string    () 文字列値 *) 
  (* | Int of int          ( 整数値 *) 

(* type expr =  *)
    (* Add of expr * expr  (a + b *)
  (* | Sub of expr * expr  (a - b *)
  (* | Const of value      (定数値 *)

(* 
  let rec eval e =
    match e with
    Add(e1, e2) ->
    begin
    match eval e1, eval e2 with
    Int(i1), Int(i2) -> Int(i1 + i2) 
    | String(s1), String(s2) -> String(s1 ^ s2) 
    | _ -> raise Type_mismatch
    end
    | Sub(e1, e2) ->
    begin
    match eval e1, eval e2 with
    Int(i1), Int(i2) -> Int(i1 - i2) 
    | _ -> raise Type_mismatch
    end
    | Const(v) -> v *)

  (* let rec len xs =
    let n in
      match xs with
        _::tl -> 
      [] -> [];; *)

(* let rec sigma xs =
  match xs with
 *)

let rec sigma xs =
  match xs with
    hd::tl -> hd +. (sigma tl)
  | [] -> 0.0;;
sigma [1.2;3.4;5.6];;

let rec find xs v =
  match xs with
    hd::tl -> 
      if hd = v then true
      else find tl v
    | [] -> false;; 
find ["iniad"; "toyo"; "univ"] "abc";;
find ["iniad"; "toyo"; "univ"] "toyo";;

(* let rec len xs =
  match xs with
    hd::tl ->
      sum () *)

(* リストの分解 リストの合計*)
let rec len xs =
  match xs with
    _::tl -> 1 + (len tl)
  | [] -> 0;;
len [1;2;3;4;5];;
len ["iniad"; "toyo"];;

(* let avg a b = 
  let a = float_of_int a ;
  let b = float_of_int b in
  if a = 0 then a +. b /. 2
  else a + b /2
avg 2 3;; *)

let avg a b = (float_of_int a +. float_of_int b) /. 2.0;;
avg 2 3;;

type pyval =
    PyInt of int      (* Pythonのint型 *)
  | PyFloat of float  (* Pythonのfloat型 *)

let rec pyval e =
  match e with
    PyInt(e) -> (eval e)
    | PyFloat(e) -> float(eval e)
    | Value(i) -> i;;
add(PyInt(1)) (PyFloat(2.3));;
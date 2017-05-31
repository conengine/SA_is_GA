open Imp;;
(* #use "imp.ml" *)

(* #load "imp.cmo";; *)
(* #use "topfind";; *)


let st0 = empty_state;;
let fuel = 7;;
let st1 = ceval_step st0 Skip fuel;; 

let st1a = ceval_step st0 (Seq (Skip, Skip)) fuel;;
let st1b = ceval_step st0 (Assign (3, (ANum 777))) fuel;;
let st1c = ceval_step st0 (Assign (1, (ANum 777))) fuel;;

let f (Some x) = x;; 

let g (Pair (a,b)) = a;;

let tes st c fuel = match ceval_step st c fuel with
    Some st -> print_endline ("Result: [" ^ string_of_int ((fst st)0) 
                               ^ " " ^     string_of_int ((fst st)1) 
                               ^ " " ^     string_of_int ((fst st)2) 
                               ^ " " ^     string_of_int ((fst st)3) 
                               ^ " ...]")
  | None -> print_endline "err.";;

let _ = tes st0 (Seq (Skip, Skip)) fuel;;
let _ = tes st0 (Assign (3, (ANum 777))) fuel;;


let cs3 = match cs2 777 with
  Some (Pair (a, b)) -> (a,b);;
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;


let tweak_string s =
  let ss = explode s in
  let rec loop = function
      []   -> EmptyString
    | h::t -> String (h, loop t)
  in loop ss;;

let rec token2bytes str = match str with
    EmptyString   -> ""
  | String (c, s) -> Char.escaped c ^ token2bytes s

let aa = tweak_string "a::=3";;
let bb = parse aa;;
let _ = bb;;

let hoge = String ('b', String ('a', EmptyString));;
let hoge2 = tweak_string "abc";;


let test s =
  print_endline s;
  let parse_res = parse (tweak_string s) in
  (match parse_res with
    NoneE _             -> print_endline ("Syntax error");
  | SomeE (Pair (c, _)) ->
      let fuel = 1000 in
      match (ceval_step cs fuel) with
        (None, _) -> print_endline ("Still running after " ^ string_of_int fuel ^ " steps")
      | (Some res, _) ->
          print_endline ("Result: [" ^ string_of_int 0
                               ^ " " ^ string_of_int 1
                               ^ " " ^ string_of_int 2
                               ^ " " ^ string_of_int 3
                               ^ " ...]"));
  print_newline();
;;

let aaa = empty_state (BId 3);;




(* test "true";; *)
(* test "SKIP";; *)
(* test "SKIP;SKIP";; *)
(* test "WHILE true DO SKIP END";; *)
(* test "x:=3";; *)
(* test "x:=3; WHILE 0<=x DO SKIP END";; *)
(* test "x:=3; WHILE 1<=x DO y:=y+1; x:=x-1 END";; *)

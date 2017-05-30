open Imp;;
(* #use "imp.ml" *)

(* #load "imp.cmo";; *)
(* #use "topfind";; *)

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


let st0 = empty_state;;

let st1 = ceval_step st0 Skip 3;; 

let st1a = ceval_step st0 (Seq (Skip, Skip)) 4;;



let cs3 = match cs2 777 with
  Some (Pair (a, b)) -> (a,b);;

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

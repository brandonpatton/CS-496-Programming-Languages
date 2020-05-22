(* Homework 3 
 * CS 496
 * I pledge my honor that I have abided by the Stevens Honor System
 *		Brandon Patton
*)

open Ast
open Ds

let is_tree = function (*Checks if input is a tree*)
	| TreeVal _ -> true
	| _ -> false

let right_tree = function (*Checks if there is a right tree*)
	| Node(_,_,rt) -> rt
	| _ -> failwith("Invalid Argument")

let left_tree = function (*Checks if there is a left tree*)
	| Node(_,lt,_) -> lt
	| _ -> failwith("Invalid Argument")

let item = function (*Checks if there is an item*)
	| Node(item,_,_) -> item
	| _ -> failwith("Invalid Argument")	

let rec eval (en:env) (e:expr):exp_val =
  match e with
  | Int n           -> NumVal n
  | Var x           -> lookup en x
  | Let(x, e1, e2)  ->
    let v1 = eval en e1  in
    eval (extend_env en x v1) e2
  | IsZero(e1)      ->
    let v1 = eval en e1  in
    let n1 = numVal_to_num v1 in
    BoolVal (n1 = 0)
  | ITE(e1, e2, e3) ->
    let v1 = eval en e1  in
    let b1 = boolVal_to_bool v1 in
    if b1 then eval en e2 else eval en e3
  | Sub(e1, e2)     ->
    let v1 = eval en e1 in
    let v2 = eval en e2  in
    NumVal ((numVal_to_num v1) - (numVal_to_num v2))

  | Add(e1, e2)     -> 
	let v1 = eval en e1 in
	let v2 = eval en e2 in
	NumVal ((numVal_to_num v1) + (numVal_to_num v2))

  | Div(e1, e2)     -> 
	let v1 = eval en e1 in
	let v2 = eval en e2 in
	NumVal ((numVal_to_num v1) / (numVal_to_num v2))

  | Mul(e1, e2)     -> 
	let v1 = eval en e1 in
	let v2 = eval en e2 in
	NumVal ((numVal_to_num v1) * (numVal_to_num v2))

  | Abs(e1)         -> 
	let v1 = eval en e1 in
	if (numVal_to_num v1) < 0
	then NumVal ((numVal_to_num v1) * ( -1))
	else NumVal (numVal_to_num v1)

  | Cons(e1, e2)    ->
	let v1 = eval en e1 in
	let v2 = eval en e2 in
	(match v2 with
	| ListVal _ ->
		ListVal ((v1)::(listVal_to_list v2))
	| _ -> failwith("Invalid Argument"))
	
  | Hd(e1)          -> 
	let v1 = eval en e1 in
	let v2 = listVal_to_list v1 in
	(match v2 with 
		| [] -> failwith("Empty List")
		| h::_ -> h )
	
  | Tl(e1)          -> 
	let v1 = eval en e1 in
	let v2 = listVal_to_list v1 in
	(match v2 with
		| [] -> failwith("Empty List")
		| _::t -> ListVal t)

  | Empty(e1)       -> (match eval en e1 with
	| ListVal v1 ->
		BoolVal(v1 = [])
						(* supports both lists and trees *)
	| TreeVal v1 ->
		BoolVal(v1 = Empty)
	| _ -> failwith("Invalid Argument"))

  | EmptyList -> ListVal []

  | EmptyTree -> TreeVal(Empty)


  | Node(e1,lt,rt)  -> 
	let v1 = eval en lt in
	let v2 = eval en rt in
	if is_tree v1 && is_tree v2  (*Helper is_tree used here defined at top*)
	then TreeVal(Node (eval en e1, treeVal_to_tree v1, treeVal_to_tree v2))
	else failwith("lt and rt must be trees")		

  | CaseT(target,emptycase,id_e,id_lt,id_rt,nodecase) -> 
	if is_tree (eval en target)
	then 	if target != EmptyTree
		then	let t = treeVal_to_tree (eval en nodecase) in    
			let en_r = extend_env en id_rt (TreeVal (right_tree t)) in (*3 helpers for each 3 parts of a node used here are defined at top*)
			let en_l = extend_env en_r id_lt (TreeVal (left_tree t)) in
		 	let en_i = extend_env en_l id_e (item t) in
			eval en_i nodecase
		else eval en emptycase
	else failwith("target is not a tree")	
	

(***********************************************************************)
(* Everything above this is essentially the same as we saw in lecture. *)
(***********************************************************************)

(* Parse a string into an ast *)
let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(* Interpret an expression *)
let interp (e:string):exp_val =
  e |> parse |> eval (empty_env ())

open Ast
(* I pledge my honor that I have abided by the Stevens Honor System 
	 Ronald Kim *)


let from_some = function
  | None -> failwith "from_some: None"
  | Some v -> v

(*  ;;;;;;;;;;;;;;;; type environments ;;;;;;;;;;;;;;;; *)

type tenv =
  | EmptyTEnv
  | ExtendTEnv of string*texpr*tenv

let empty_tenv () = EmptyTEnv

let extend_tenv id t tenv = ExtendTEnv(id,t,tenv)


let rec apply_tenv (tenv:tenv) (id:string):texpr option =
  match tenv with
  | EmptyTEnv -> None
  | ExtendTEnv (key,value,tenv1) ->
    if id=key
    then Some value
    else apply_tenv tenv1 id


let init_tenv () =
     extend_tenv "x"  IntType
     @@ extend_tenv "v" IntType
     @@ extend_tenv "i"  IntType
     @@ empty_tenv ()

let rec  string_of_tenv  = function
  | EmptyTEnv -> ""
  | ExtendTEnv(id,v,env) -> "("^id^","^string_of_texpr v^")"^string_of_tenv env



let rec type_of_prog = function
  | AProg e -> type_of_expr (init_tenv ()) e
and
  type_of_expr en = function
  | Int n          -> IntType
  | Var id          ->
    (match apply_tenv en id with
    | None -> failwith @@ "Variable "^id^" undefined"
    | Some texp -> texp)
  | Unit ->
    UnitType
  | ITE(e1, e2, e3)    ->
    let t1 = type_of_expr en e1
    in let t2 = type_of_expr en e2
    in let t3 = type_of_expr en e3
    in if t1=BoolType && t2=t3
    then t2
    else failwith "ITE: Type error"
  | Add(e1, e2) | Mul(e1,e2) | Sub(e1,e2) | Div(e1,e2)    ->
    let t1 = type_of_expr en e1 in
    let t2 = type_of_expr en e2  in
    if t1=IntType && t2=IntType
    then IntType
    else failwith "Add: arguments must be ints"
  | IsZero(e) ->
    let t1 = type_of_expr en e  in
    if t1=IntType
    then BoolType
    else failwith "Zero?: argument must be int"
  | Let(x, e1, e2) ->
    let t1 = type_of_expr en e1
    in type_of_expr (extend_tenv x t1 en) e2
  | Proc(x,ty,e)      ->
    let tc= type_of_expr (extend_tenv x ty en) e
    in FuncType(ty,tc)
  | App(e1,e2)     ->
    let t1 = type_of_expr en e1
    in let t2 = type_of_expr en e2
    in (match t1 with
    | FuncType(td,tcd) when td=t2 -> tcd
    | FuncType(td,tcd) -> failwith "App: argument does not have correct type"
    | _ -> failwith "Checker: App: LHS must be function type")
  | Letrec(tRes,id,param,tParam,body,e) ->
    let t=type_of_expr (extend_tenv param tParam
                          (extend_tenv id (FuncType(tParam,tRes)) en))
        body
    in if t=tRes
    then type_of_expr (extend_tenv id (FuncType(tParam,tRes)) en) e
    else failwith
        "Checker: LetRec: Types of recursive function does not match declaration"
  | Set(id,e) ->
      failwith "EXPLICIT-REFS: Set not a valid operation"
  | BeginEnd(es) ->
    List.fold_left (fun v e -> type_of_expr en e) UnitType es

  (* explicit ref *)
  | NewRef(e) -> RefType (type_of_expr en e)
  | DeRef(e) ->
        (match type_of_expr en e with 
          | RefType(t1) -> t1
          | _ -> failwith "Incorrect input"
      ) 
  | SetRef(e1,e2) ->
    (match (type_of_expr en e1) with
        | RefType(t1) ->
            let t2 = type_of_expr en e2 in
            if(t2 = t1)
            then UnitType
          else failwith "e1 and e2 do not match"
        | _ -> failwith  "Incorrect input"
  )

  (* pair *)
  | Pair(e1, e2) -> 
	PairType ((type_of_expr en e1),(type_of_expr en e2)) 
  | Unpair(id1, id2, def, body) ->
    (match def with
      | Pair(a, b) -> type_of_expr (extend_tenv id1 (type_of_expr en a) (extend_tenv id2 (type_of_expr en b) en)) body
      | _ -> failwith "Incorrect format: Not a Pair")

  (* list *)
  | EmptyList(t) -> ListType(t)
  | Cons(he, te) ->
    (match (type_of_expr en te) with
      | ListType(a) -> ListType(type_of_expr en he) 
      | _ -> failwith "Incorrect input")
  | Null(e) ->
    (match (type_of_expr en e) with
      | ListType(a) -> BoolType
      | _ -> failwith "Incorrect input")
  | Hd(e) -> 
    (match (type_of_expr en e) with
      | ListType(a) -> a
      | _ -> failwith "Incorrect input")
  | Tl(e) ->
    (match (type_of_expr en e) with
      | ListType(a) -> ListType(a)
      | _ -> failwith "Incorrect input")

  (* tree *)
  | EmptyTree(t) -> TreeType(t)
  | Node(de, le, re) ->
    (match (type_of_expr en de) with
      | TreeType(t) -> failwith "Incorrect input"
      | _ -> 
        (match (type_of_expr en le) with
          | TreeType(t) -> 
              (match (type_of_expr en re) with
                | TreeType(t) -> TreeType(type_of_expr en de)
                | _ -> failwith "Incorrect input")
          | _ -> failwith "Incorrect input"))
  | NullT(t) ->
    (match (type_of_expr en t) with
      | TreeType(r) -> BoolType
      | _ -> failwith "Incorrect input")
  | GetData(t) ->
    (match (type_of_expr en t) with
      | TreeType(r) -> r
      | _ -> failwith "Incorrect input")
  | GetLST(t) ->
    (match (type_of_expr en t) with
      | TreeType(l) -> TreeType(l)
      | _ -> failwith "Incorrect input")
  | GetRST(t) ->
    (match (type_of_expr en t) with
      | TreeType(r) -> TreeType(r)
      | _ -> failwith "Incorrect input")


  | Debug ->
    print_string "Environment:\n";
    print_string @@ string_of_tenv en;
    UnitType



let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(* Interpret an expression *)
let chk (e:string) : texpr =
  e |> parse |> type_of_prog


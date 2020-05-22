(*I pledge my honor that I have abided by the Stevens Honor System
	Brandon Patton*)

type program = int list

let rec dupe = fun start -> match start with
	(*Handles duplicates*) 
	| [] -> []
	| h::t -> h:: dupe (List.filter (fun a -> h<>a)t);;

let rec mover = fun (x,y) moves check -> match moves with
	(*performs the moves as described by the move list*)
	| [] -> []
	| h::t -> match h with
	| 0 -> (x,y)::mover (x,y) t true
	| 1 -> mover (x,y) t false
	| 2 ->
		if (check = true) 
		then (x,y+1)::mover (x,y+1) t true
		else mover (x,y+1) t check
	| 3 ->
		if (check = true)
		then (x+1,y)::mover (x+1,y) t true
		else mover (x+1,y) t check
	| 4 ->
		if (check = true)
		then (x,y-1)::mover (x,y-1) t true
		else mover (x,y-1) t check
	| 5 ->
		if (check = true)
		then (x-1,y)::mover (x-1,y) t true
		else mover (x-1,y) t check
	| _ -> failwith "argument is not valid";;

let rec colored : int*int -> program -> (int*int) list = fun (x,y) moves ->
	(*Takes in a starting coordinate and a program (int list) and returns the list of coordinates that the program has colored using its pen.  Also removes repeated elements from the list.*)
	dupe (mover (x,y) moves false);;

let rec equivalent_help : 'a list -> 'a list -> bool = fun (a: 'a list) (b: 'a list) ->
	(*Checks if two lists are equivalent*)
	if ((a = [] && b <> []) || (a <> [] && b = []) || (List.length a <> List.length b))
	then false
	else match a with
		| [] -> if b = [] 
			then true
			else false
		| h::t -> equivalent_help t (List.filter(fun x -> x <> h) b);;

let equivalent : program -> program -> bool = fun a b ->
	(*Checks whether two programs are equivalent, that is, if they color the same set of coordinates*)
	if equivalent_help (colored (0,0) a) (colored (0,0) b)
	then true
	else false

let rec repeat : int -> 'a -> 'a list = fun n x -> match n with
	(*Returns a list with n copies of x*)
	| 0 -> []
	| _ -> x::(repeat (n-1) x)		

let rec rotate_90 : program -> program = fun moves -> match moves with
	(*Takes in a program (int list) and returns a new one which draws the same pictures except that they are rotated 90 degrees clockwise*)
	| [] -> []
	| [x] ->
		if x = 2
		then [3]
		else if x = 3
		then [4]
		else if x = 4
		then [5]
		else if x = 5
		then [2]
		else [x]		
	| h::t ->
		if h = 2
		then 3::rotate_90 t
		else if h = 3
		then 4::rotate_90 t
		else if h = 4
		then 5::rotate_90 t
		else if h = 5
		then 2::rotate_90 t
		else h::rotate_90 t

let rec mirror_image : program -> program = fun moves ->
	(*Returns a program (int list) that draws the mirror image of the input program. Uses rotate_90 to achieve this*)
	rotate_90 (rotate_90 moves);;
	
let rec pantograph : program -> int -> program =  fun p n -> match p with
	(* Returns a program (int list) that draws the same things as p only enlarged n-fold, uses repeat*)
	| [] -> []
	| [x] ->
		repeat n x		
	| h::t ->
		(repeat n h)@(pantograph t n);;

let rec compress : program -> (int*int) list = fun moves -> 
	(*Compresses a program by replacing adjacent copies of the same instruction with a tuple (m, n) where m is the instruction and n is the number of consecutive times it should be executed*)
	let rec compress_help count compressed = function
		| [] -> []
		| [x] ->
			(x, count + 1) :: compressed
		| a :: (b :: _ as t) ->
					if a = b
					then compress_help (count + 1) compressed t
					else compress_help 0 ((a, count + 1) :: compressed) t
	in List.rev (compress_help 0 [] moves);;

let rec decompress : (int*int) list -> program = fun compressed -> match compressed with
	(*Decompresses a compressed program*)
	| [] -> []
	| (m, n)::t -> repeat n m @ decompress t;;









	

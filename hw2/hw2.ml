(* Homework Assignment 2
 * CS 496
 * I pledge my honor that I have abided by the Stevens Honor System.
 * Brandon Patton
 *)

type dTree = Leaf of int | Node of char * dTree * dTree
exception Invalid_argument;;

let tLeft = Node('w', Node('x', Leaf 2, Leaf 5), Leaf 8)
let tRight = Node('w', Node('x', Leaf 2, Leaf 5), Node('y', Leaf 7, Leaf 5))

let rec dTree_height = fun height -> match height with
	(*Returns the height of a given dTree*)
	| Leaf i -> 0
	| Node(x, lt, rt) ->
		if dTree_height lt > dTree_height rt
		then dTree_height lt + 1
		else dTree_height rt + 1

let rec dTree_size = fun size -> match size with
	(*Returns the size of a given dTree. The size consists of the number of nodes and leaves*)
	| Leaf i -> 1
	| Node(x, lt, rt) -> 
		dTree_size lt + dTree_size rt + 1

let rec dTree_paths d = 
	(*Returns a list with all the paths to a given dTree's leaves. A path is a list of digits in the set {0, 1} when followed leads to a leaf. Order is irrelevant. Uses a helper function to concatenate paths*)
	let rec concat paths lst = match lst with
		| [] -> []
		| h::t -> 
			[paths::h]@concat paths t in
	match d with
	| Leaf(x) -> [[]]
	| Node(m, l, r) ->
		let lt = concat 0 (dTree_paths l) in
			let rt = concat 1 (dTree_paths r) in
				lt@rt

let rec isPerfect = fun fst snd -> match (fst, snd) with
	(*Simple comparison to be used in dTree_is_perfect*)
	(true, true) -> true
	| _ -> false

let rec dTree_is_perfect = fun perfect -> match perfect with
	(*Determines if a given dTree is perfect. Perfect means all leaves have same depth*)
	| Leaf i -> true
	| Node(x, lt, rt) ->
		if (dTree_height lt = dTree_height rt)
		then isPerfect (dTree_is_perfect lt) (dTree_is_perfect rt)
		else false

let rec dTree_map = fun f g t -> match t with
	(*Returns a new dTree given another dTree t by applying f to the characters in each node and applying g to the numbers in each leaf*)
	| Leaf i -> Leaf (g i)
	| Node(x, lt, rt) -> 
		Node(f x, dTree_map f g lt, dTree_map f g rt)

let rec list_to_tree = fun l -> match l with
	(*Creates a tree from a list of characters l. The symbols of an inner node at level n corresponds to the n-th element in l*)
	| [] -> Leaf 0
	| h::t -> Node(h, list_to_tree t, list_to_tree t)

let rec edit = fun f t s -> match f,t with
	(*Handles the actual editing of each leaf. Used in replace_leaf_at*)
	| [], Leaf(x) -> Leaf(s)
	| 0::xs, Node(a, lt, rt) -> 
		Node(a, (edit xs lt s), rt)
	| 1::xs, Node(a, lt, rt) -> 
		Node(a, lt, (edit xs rt s))
	| _ -> 
		failwith "Unexpected Input"

let rec replace_leaf_at = fun t f -> match f with
	(*Replaces all the leaves in t by the value indicated in f which is a graph of a function*)
	| [] -> t
	| (x, y)::zs -> 
		replace_leaf_at (edit x t y) zs

let bf_to_tree = fun f -> match f with
	(*Takes a pair-encoding of a boolean function and returns its tree-encoding*)
	| (f,l) -> 
		replace_leaf_at (list_to_tree f) l




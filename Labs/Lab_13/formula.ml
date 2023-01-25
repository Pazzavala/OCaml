type formula = Bool of bool (* Boolean literals of true and false *)
							| And  of formula * formula
	     				| Implies of formula * formula
           	  | Or of formula * formula
	     				| Not of formula 
	     				| Prop of string (* variables that are assigned Boolean values of true or false *)
	     			
and subst = (string * bool) list

exception UnboundVariable of string

(* Function used in class - Van Wyk *)
let rec lookup (x: string) (subset: subst) =
  match subset with
  | [] -> raise (UnboundVariable x)
  | (name,boolean):: rest when x = name -> boolean
  | _ :: rest -> lookup x rest

let rm_duplicates (lst: 'a list) : 'a list =
	let collect_unique elem to_keep =
		if List.mem elem to_keep then to_keep else elem::to_keep
	in List.fold_right collect_unique lst []
	
	

let rec eval (f: formula) (subset: subst) : bool =
	match f with
	| Bool b -> b
	| Not (f) -> (match eval f subset with
							 | b -> not b) 
	| And (f1, f2) -> (match eval f1 subset, eval f2 subset with
										| b1, b2 -> b1 && b2)
	| Or (f1, f2) -> (match eval f1 subset, eval f2 subset with
									 | b1, b2 -> b1 || b2)
	| Implies (f1, f2) -> (match eval f1 subset, eval f2 subset with
												| b1, b2 -> not b1 || b2)
	| Prop (x) -> lookup x subset

let freevars (f: formula) : string list =
	let rec build f = 
		match f with
		| Bool b -> []
		| Prop x -> [x]
		| Not (f) -> build f
		| And (f1, f2) | Or (f1, f2) | Implies (f1, f2) -> build f1 @ build f2
	in rm_duplicates (build f)
	

(* Here is a function for converting substitutions to strings. 
	 We'll use this in printing our results *)
let show_list (show: 'a -> string) (lst: 'a list) : string =
  let rec sl lst =
    match lst with 
    | [] -> ""
    | [x] -> show x
    | x::xs -> show x ^ "; " ^ sl xs
  in "[ " ^ sl lst ^ " ]"

let show_string_bool_pair (s,b) =
  "(\"" ^ s ^ "\"," ^ (if b then "true" else "false") ^ ")"

let show_subst (s: subst) : string = show_list show_string_bool_pair s

(* Other helpful functions *)
let explode (s: string) : char list =
  let len = String.length s in
  let rec f i = if i = len then [] else s.[i] :: f (i+1) in
  f 0

let _ =
	assert(eval (And ( Prop "P", Prop "Q")) [("P",true); ("Q",false)] = false);
	assert(eval (And ( Prop "P", Prop "Q")) [("P",true); ("Q",true)] = true)

let _ =
	assert(freevars (And ( Prop "P", Prop "Q")) = ["P"; "Q"]);
	assert(freevars (And ( Prop "Q", Prop "P")) =  ["Q"; "P"]);
	assert(List.length (freevars (Or (And ( Prop "P", Prop "Q"), Implies (Prop "R", Prop "Q")))) = 3);
	assert(List.mem "P" (freevars (Or (And ( Prop "P", Prop "Q"), Implies (Prop "R", Prop "Q")))) = true);
	assert(List.mem "Q" (freevars (Or (And ( Prop "P", Prop "Q"), Implies (Prop "R", Prop "Q")))) = true);
	assert(List.mem "R" (freevars (Or (And ( Prop "P", Prop "Q"), Implies (Prop "R", Prop "Q")))) = true) 
(* Maria Zavala - Literacy *)

type 'a tree = Empty
| Fork of 'a tree * 'a * 'a tree

let rec reduce (e: 'b) (f: 'b -> 'a -> 'b -> 'b) (t: 'a tree) : 'b =
  match t with
  | Empty -> e
  | Fork (lt, v, rt) -> f (reduce e f lt) v (reduce e f rt)

let rec longest_string_tree (t: string tree) =
  let max_string s1 s2 = 
    match compare (String.length s1) (String.length s2) with
    | -1 -> s2
    | 0 | 1 -> s1
    | _ -> raise (Failure "IDK")
  in
  match t with
  | Empty -> None
  | Fork (lt, s, rt) -> 
    match longest_string_tree lt, longest_string_tree rt with
    | (None,None) -> Some s
    | (Some ls, None) -> Some (max_string ls s)
    | (None, Some rs) -> Some (max_string s rs)
    | (Some ls, Some rs) -> Some (max_string (max_string ls s) rs)


let rec sum_pairs (t: (int * int) tree) : int =
  match t with
  | Empty -> 0
  | Fork (lt, (v1, v2), rt) -> sum_pairs lt + (v1 + v2) + sum_pairs rt


let rec max_pairs (t: ('a * 'a) tree) : 'a option=
  match t with
  | Empty -> None
  | Fork (lt, (v1, v2), rt) -> 
    match max_pairs lt, max_pairs rt with
    | (None, None) -> Some (max v1 v2)
    | (Some lv, None) -> Some (max (max v1 v2) lv)
    | (None, Some rv) -> Some (max (max v1 v2) rv)
    | (Some lv, Some rv) -> Some (max (max (max v1 v2) lv) rv)

let rec unzip (t: ('a * 'b) tree): ('a tree * 'b tree) =
  match t with
  | Empty -> (Empty, Empty)
  | Fork (lt, (a, b), rt) -> 
    match unzip lt, unzip rt with
    |  ((la, lb) , (ra, rb)) -> (Fork (la, a, ra) ,(Fork (lb, b, rb)))

let sum_tree_reduce (t: int tree) : int =
  reduce 0 (fun lv v rv -> lv + v + rv) t

let max_tree_reduce (t: int tree) : int option =
  let maximum (lopt: int option) (v1: int) (ropt: int option) : int option =
    match lopt, ropt with
    | (None, None) -> Some v1
    | (Some v2, None) -> Some (max v1 v2)
    | (None, Some v3) -> Some (max v1 v3)
    | (Some v2, Some v3) -> Some (max (max v1 v2) v3)
  in
  reduce None maximum t
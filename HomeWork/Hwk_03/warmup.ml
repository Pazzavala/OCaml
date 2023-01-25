(* Maria Zavala - Warm up *)

type 'a tree = Empty
| Fork of 'a tree * 'a * 'a tree

type 'a rose_tree =
  Rose of 'a * 'a rose_tree list


let rec min_list (lst: 'a list) : 'a option =
  match lst with
  | [] -> None
  | x :: xs -> match min_list xs with
               | None -> Some x
               | Some y -> Some (min x y)

let rec longest_string_list (strs: string list) : string option =
  match strs with
  | [] -> None
  | s1 :: rest -> 
    match longest_string_list rest with
    | None -> Some s1
    | Some s2 -> if (String.length s1) < (String.length s2)
                  then Some s2
                 else Some s1 


let rec sum_tree (t: int tree) : int =
  match t with
  | Empty -> 0
  | Fork (lt, v, rt) -> sum_tree lt + v + sum_tree rt

let rec max_tree (t: int tree) : int option =
  match t with
  | Empty -> None
  | Fork (lt, v1, rt) -> match max_tree lt, max_tree rt with
                       | (None ,None) -> Some v1
                       | (Some v2, Some v3) -> Some (max (max v1 v2) v3)
                       | (Some v2, None) -> Some (max v1 v2)
                       | (None, Some v3) -> Some(max v1 v3)

let rec max_rose (Rose (v, rs): int rose_tree) : int =
  List.fold_left max v (List.map max_rose rs)
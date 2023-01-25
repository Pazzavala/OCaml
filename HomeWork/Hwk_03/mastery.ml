(* Maria Zavala - Mastery *)

type 'a tree = Empty
| Fork of 'a tree * 'a * 'a tree

type 'a zom_tree = Zero
                  | One of 'a
                  | Many of 'a * 'a zom_tree * 'a zom_tree

let rec reduce (e: 'b) (f: 'b -> 'a -> 'b -> 'b) (t: 'a tree) : 'b =
  match t with
  | Empty -> e
  | Fork (lt, v, rt) -> f (reduce e f lt) v (reduce e f rt)

  
let sum_pairs_reduce (t: (int * int) tree) : int =
  reduce 0 (fun lv (v1, v2) rv -> lv + v1 + v2 + rv) t

let max_pairs_reduce (t: ('a * 'a) tree) : 'a option =
  let maximum lr (v1, v2) rr = 
    match lr, rr with
    | (None, None) -> Some (max v1 v2)
    | (Some lv, None) -> Some (max (max v1 v2) lv)
    | (None, Some rv) -> Some (max (max v1 v2) rv)
    | (Some lv, Some rv) -> Some (max (max (max v1 v2) lv) rv)
  in
  reduce None maximum t

let unzip_reduce (t: ('a * 'b) tree) : ('a tree * 'b tree) =
  let separate lr (a, b) rr = 
    match lr, rr with
    | ((la, lb), (ra, rb)) -> (Fork (la, a, ra) ,(Fork (lb, b, rb)))
  in
  reduce (Empty, Empty) separate t

let rec reduce_zom (e: 'b) (o: 'a -> 'b) (f: 'a -> 'b -> 'b -> 'b) (z: 'a zom_tree) : 'b =
  match z with
  | Zero -> e
  | One v1 -> o v1
  | Many (v, zom1, zom2) -> f v (reduce_zom e o f zom1) (reduce_zom e o f zom2)

let sum_zom (z: int zom_tree) : int =
  reduce_zom 0 (fun v1 -> v1) (fun v z1 z2 -> v + z1 + z2) z
  

let max_zom (z: 'a zom_tree) : 'a option = 
  let maximum v1 z1 z2 = match z1, z2 with
                        | (None, None) -> Some v1
                        | (Some v2, None) -> Some (max v1 v2)
                        | (None, Some v3) -> Some (max v1 v3)
                        | (Some v2, Some v3) -> Some (max (max v1 v2) v3)
  in
  reduce_zom None (fun v1 -> Some v1) maximum z

let square_all_zom (z: int zom_tree) : int zom_tree =
  let square v z1r z2r = Many (v * v, z1r, z2r)
  in
  reduce_zom Zero (fun v1 -> One (v1 * v1)) square z

  
(* Cite solutions if used in class *)

let dec (num: int) : int = num - 1

let cube (num: int) : int = num * num * num

let last_digit (num: int) : int = abs (num mod 10)

let rec sum (lst: int list) : int =
  match lst with
  | [] -> 0
  | x :: rest -> x + sum rest

let rec product (lst: int list) : int =
  match lst with
  | [] -> 1
  | x :: rest -> x * product rest

let rec all_odds (lst: int list) : int list =
  match lst with
  | [] -> []
  | x :: rest when x mod 2 = 1 -> x :: all_odds rest
  | _ :: rest -> all_odds rest

let rec square_all (lst: int list) : int list =
  match lst with
  | [] -> []
  | x :: rest -> x * x :: square_all rest

  
(* Level 2 *)
  
let rec first_digit (num: int ) : int =
  if abs num < 10
    then abs num
  else first_digit (num / 10)

let product_to (n: int) : int =
  let rec accumulator product_so_far i =
    if n < 0
      then raise (Failure "invalid input to product_to")
    else if n = 0
      then 0
    else if i > n
      then product_so_far
    else accumulator (product_so_far * i) (i + 1)
  in accumulator 1 1

let is_prime (num: int) : bool =
  let rec decrement i =
    if num < 0
      then raise (Failure "invalid input to is_prime")
    else if num = 1 || num = 0
      then false
    else if i = 1
      then true
    else if (num mod i) = 0
      then false 
    else decrement (i - 1)  
  in decrement (num - 1)

let rec last_digits (lst: int list) : int list =
  match lst with
  | [] -> []
  | x :: rest -> (last_digit x) :: last_digits rest

let rec all_primes (lst: int list) : int list =
  match lst with
  | [] -> []
  | x :: rest when (is_prime x) -> x :: all_primes rest
  | _ :: rest -> all_primes rest 

let rec sum_pairs (lst: (int * int) list) : int =
  match lst with
  | [] -> 0
  | (x1, x2) :: rest -> x1 + x2 + sum_pairs rest

(* Used in class made by Van Wyk (2022)  find_and_lookup.ml [Source Code]*)
let max_cmp (cmp: 'a -> 'a -> int) (lst: 'a list) : 'a =
  let rec max_accum (max_so_far: 'a) (vals: 'a list) =
    match vals with
    | [] -> max_so_far
    | v :: vs -> match cmp v max_so_far with
                  | 0 | -1 -> max_accum max_so_far vs
                  | 1 -> max_accum v vs
                  | _ -> raise (Failure "internal error: this should not happen")
  in
  match lst with
  | [] -> raise (Failure "invalid input to max_pairs")
  | x :: xs -> max_accum x xs

let max_pair (lst: (int * int) list) : (int * int) =
  let cmp (n1) (n2)= 
    compare n1 n2
  in max_cmp cmp lst

let max_pairs (lst: (int * int) list) : int =
  let (x1, x2) = max_pair lst in
  if x1 > x2 then x1 else x2


(* Level 3 *)

let is_prime_faster (num: int) : bool =
  let rec decrement i =
    let i = int_of_float (Float.sqrt (float_of_int i)) in
    if num < 0
      then raise (Failure "invalid input to is_prime")
    else if num = 1 || num = 0
      then false
    else if i = 1
      then true
    else if (num mod i) = 0
      then false 
    else decrement (i - 1)  
  in decrement (num)

let square (x: int) : int = x * x

let rec big_squares (lst: int list) : int list =
  match lst with
  | [] -> []
  | x :: rest when square x >= 50 -> square x :: big_squares rest
  | _ :: rest -> big_squares rest

let rec sum_of_squares (lst : int list) : int =
  match lst with
  | [] -> 0
  | x :: rest -> square x + sum_of_squares rest 

let rec product_of_primes (lst: int list) : int =
  match lst with
  | [] -> 1
  | x :: rest when (is_prime x) -> x * product_of_primes rest
  | _ :: rest -> product_of_primes rest

(* Modified - Used in class made by Van Wyk (2022)  find_and_lookup.ml [Source Code]*)
let max_cmp_list (cmp: 'a -> 'a -> int) (lst: 'a list) : 'a list=
let rec max_accum (max_so_far: 'a) (vals: 'a list) =
  match vals with
  | [] -> [max_so_far]
  | v :: vs -> match cmp v max_so_far with
                (* can combine say if 0 or -1 *)
                | 0 -> max_accum v vs @ [max_so_far]  
                | -1 -> max_accum max_so_far vs
                | 1 -> max_accum v vs
                | _ -> raise (Failure "internal error: this should not happen")
  in
  match lst with
  | [] -> raise (Failure "invalid input to max_int_string_pairs")
  | x :: xs -> max_accum x xs  

let max_int_string_pairs (lst: (string * int) list) : (string * int) list =
  let cmp (_, n1) (_, n2)= 
    compare n1 n2
  in max_cmp_list cmp lst

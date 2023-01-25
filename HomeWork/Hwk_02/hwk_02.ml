(* Homework 2: High order Functions - Maria Zavala *)


(* Warm up *)

let sum (nums: int list) : int =
  List.fold_left ( + ) 0 nums

let product (nums: int list) : int =
  List.fold_right ( * ) nums 1

let all_odds (nums: int list) : int list =
  List.filter (fun x -> x mod 2 = 1) nums

let square_all (nums: int list) : int list =
  List.map (fun x -> x * x) nums

let lengths (strs: string list) : int list =
  List.map String.length strs

let length_pairs (strs: string list) : (string * int) list =
  (* need a function that returns tuples *)
  let pairs str = (str, String.length str)
  in List.map pairs strs


(* Literacy *)

let last_digits (nums: int list) : int list =
  let last_digit num = abs (num mod 10)
  in List.map last_digit nums

let all_primes (nums: int list) : int list =
  let is_prime num =
    let rec decrement i =
      if num < 0
        then raise (Invalid_argument "Invalid input to is_prime")
      else if num = 1 || num = 0
        then false
      else if i = 1
        then true
      else if (num mod i) = 0
        then false 
      else decrement (i - 1)  
    in decrement (num - 1)
  in
  List.filter is_prime nums

let sum_pairs (pairs: (int * int) list) : int =
  (* Need a function that takes pairs and adds them together *)
  let add_pair (n1, n2) n3 = n1 + n2 + n3
  in List.fold_right add_pair pairs 0

let max_pairs (pair_lst: (int * int) list): int =
  match pair_lst with
  | [] -> raise (Invalid_argument "invalid input to max_pairs")
  | x :: xs -> match List.fold_right max xs x with
               | (x1, x2) -> max x1 x2

let rev (lst: 'a list) : 'a list =
  let f list_so_far elem = elem :: list_so_far
  in
  List.fold_left f [] lst

let elem (a: 'a) (lst: 'a list) : bool =
  let f rest elem = elem = a || rest
  in
  List.fold_left f false lst

let unzip (pair_lst: ('a * 'b) list) : 'a list * 'b list =
  let f (x1, x2) (lst1, lst2) = (x1 :: lst1, x2 :: lst2)
  in
  List.fold_right f pair_lst ([], [])   


(* Mastery *)

let square x = x * x

let big_squares (nums: int list) : int list =
  List.filter (fun num -> num >= 50) (List.map square nums)

let sum_of_squares (nums: int list) : int =
  List.fold_left ( + ) 0 (List.map square nums)

let product_of_primes (nums: int list) : int =
  List.fold_left ( * ) 1 (all_primes nums)

 let max_int_string_pairs (pairs_lst: (string * int) list) : (string * int) list =
  match pairs_lst with
  | [] -> raise (Invalid_argument "invalid input to max_int_string_pairs")
  | _ -> let find_max ((s1, n1): (string * int)) (list_so_far: (string * int) list)=
          match list_so_far with
          | [] -> [(s1, n1)]
          | (s2, n2) :: xs -> match compare n2 n1 with
                              | -1 -> [(s1, n1)]
                              | 0 -> (s1, n1) :: list_so_far
                              | 1 -> list_so_far
                              | _ -> raise (Failure "internal error")
          in
          List.fold_right find_max pairs_lst []

let group (lst: 'a list) : ('a * 'a) list =
  let f elem (buffer, blst) = 
    match buffer with
    | [x] -> ([], (elem, x) :: blst)
    | _ -> (elem :: buffer, blst)
  in
  match List.fold_right f lst ([], []) with
  | ([], blst) -> blst
  | (buffer, blst) -> 
    raise (Invalid_argument "List has an odd number of elements")

let rec range (from: int) (up_to: int) : int list =
  let inc x = x + 1 
  in
  match from <= up_to with
  | false -> []
  | true -> from :: range (inc from) up_to

let sieve (x: int) (nums: int list) : int list =
  List.filter (fun num -> num mod x != 0) nums

let primes (lst: int list) : int list = 
  match lst with
  | x :: xs when x < 2 -> raise (Invalid_argument "Must start with 2 or []")
  | _ -> let build (num: int) (primes_lst) = num :: sieve num primes_lst
         in
         List.fold_right build lst [] 

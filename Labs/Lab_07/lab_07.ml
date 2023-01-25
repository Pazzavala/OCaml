type 'a tree = Empty
             | Fork of 'a tree * 'a * 'a tree

let rec reduce (e: 'b) (f: 'b -> 'a -> 'b -> 'b) (t: 'a tree) : 'b =
  match t with
  | Empty -> e
  | Fork (lt, v, rt) -> f (reduce e f lt) v (reduce e f rt)

let size (t: 'a tree) : int =
  let len lv v rv = lv + 1 + rv
  in
  reduce 0 len t
(* t2 = 6 ? *)

let sum (t: 'a tree) : int =
  let add3 lv v rv = lv + v + rv
  in
  reduce 0 add3 t

let product (t: 'a tree) : int =
  let mul lv v rv = lv * v * rv
  in
  reduce 1 mul t

let char_count (t: string tree) : int =
  let str_length lv v rv = lv + (String.length v) + rv
  in
  reduce 0 str_length t

let perfect_balance (t: 'a tree) : bool =
  let helper (lb, lh) elem (rb, rh) = 
    (lb && rb && lh = rh, (max lh rh) + 1) 
  in 
  match reduce (true, 0) helper t with
  | (b, h) -> b

let maximum (t: 'a tree) : 'a option =
  let max3 lv v rv = max (max lv rv) (Some v)
  in
  reduce None max3 t



  
  (* Practice *)
let same_length (t: string tree) : bool =
  let check (lb, ll) elem (rb, rl) =
    (lb && rb && ll = rl, String.length elem)
  in
  match reduce (true, 0) check t with
  | (b, l) -> b

let t1 : int tree = 
  Fork (
    Fork (
      Empty, 
      2, 
      Fork (Empty, 6, Empty)),
    3,
    Fork ( 
      Fork (Empty, 7, Empty),
      4,
      Fork (Empty, 5, Empty)
    )
  )

let t2 : string tree = 
  Fork (
    Fork (
      Empty, 
      "34", 
      Fork (Empty, "567", Empty)),
    "12",
    Fork ( 
      Fork (Empty, "1112", Empty),
      "8910",
      Fork (Empty, "13", Empty))
  )

let t3 : int tree =
  Fork (
    Fork ( 
      Fork (
        Fork (Empty, 0, Empty), 
        3, 
        Fork (Empty, 0, Empty)),
      2,
      Fork (
        Fork (Empty, 0, Empty), 
        4, 
        Fork (Empty, 0, Empty))),
    1,
    Fork ( 
      Fork (
        Fork (Empty, 0, Empty), 
        6, 
        Fork (Empty, 0, Empty)),
      5,
      Fork (
        Fork (Empty, 0, Empty), 
        7, 
        Fork (Empty, 0, Empty)))
    )

let t4 : 'a tree = Empty

let t5 : int tree = 
  Fork (
        Fork (
          Empty, 
          (-2),
            Fork (Empty, 6, Empty)),
        3,
        Fork ( 
              Fork (Empty, (-7), Empty),
              4,
              Fork (Fork (Empty, 19, Empty), 5, Empty)
          )
    )

let t6 : string tree =
  Fork (
    Fork (
      Fork (Empty, "hey", Empty), 
      "hi", 
      Fork(Empty, "heya", Empty)),
    "hello",
    Fork (
      Fork (Empty, "greetings", Empty), 
      "howdy", 
      Fork (Empty, "hiya", Empty)))

let t7 : 'a tree =
  Fork (Fork (Empty, "true", Empty), "true", Fork (Empty , "true", Empty) )

(* Test cases from the lab writeup *)
let _ = 
  assert (size t2 == 6);
  assert (sum t1 == 27);
  assert (product t1 == 5040);
  assert (char_count t2 == 17);
  assert (perfect_balance t1 == false);
  assert (perfect_balance t3 == true);
  assert (maximum t6 = Some "howdy");
  assert (maximum t4 == None)
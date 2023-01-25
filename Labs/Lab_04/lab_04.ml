type fraction = int * int

let mul ((n1, d1): fraction) ((n2, d2): fraction) : fraction = 
  (n1*n2, d1*d2 )

(* Used in class made by Van Wyk (2022)  9_16.md [Source Code]*)
let gcd (x: int) (y: int) : int = 
  let min = if x < y then x else y
  in
  let rec decrement i =
    if x mod i = 0 && y mod i = 0
      then i
    else decrement (i - 1)
  in
  decrement min

let simplify ((n1, d1): fraction) : fraction =
  let divisor = gcd n1 d1 in
  (n1 / divisor, d1 / divisor)

let rec compute_lengths (lst: string list) : (string * int) list =
  match lst with
  | [] -> []
  | str :: rest ->  (str, String.length str) :: compute_lengths rest

let rec make_strings (lst: (char * int) list) : string list =
  match lst with
  | [] -> []
  | (c, i) :: rest -> String.make i c :: make_strings rest

type point = float * float

let distance ((x1, y1): point) ((x2, y2): point) : float =
  let a = x1 -. x2 in
  let b = y1 -. y2 in
  let c = a *. a +. b *. b in
  Float.sqrt (Float.abs c)

type triangle = point * point * point

let perimeter (p1, p2, p3: triangle) : float =
  (distance p1 p2) +. (distance p2 p3) +. (distance p3 p1)

let rec triangle_perimeters (triangles: triangle list) : float list =
  match triangles with
  | [] -> []
  | t :: rest -> perimeter t :: triangle_perimeters rest

let _ =
  assert(mul (2,1) (3,1) = (6,1));
  assert(mul (2,3) (3,4) = (6,12));
  assert(simplify (2,6) = (1,3));
  assert(simplify (1,4) = (1,4));
  assert(simplify (2,3) = (2,3));
  assert(simplify (mul (2,3) (3,4)) = (1,2));
  assert(compute_lengths [ "Hello"; "Hi"; "" ] = [ ("Hello", 5); ("Hi", 2); ("", 0) ]);
  assert(compute_lengths [ ] = [ ]);
  assert(make_strings [ ] = [ ]);
  assert(make_strings [ ('a', 3); ('x', 0); ('4', 4) ] = [ "aaa"; ""; "4444" ]);
  assert(distance (1.0, 1.0) (2.0, 2.0) -. 1.414 < 0.001);
  assert(distance (2.0, 2.0) (1.0, 1.0) -. 1.414 < 0.001);
  assert(distance (1.0, 1.0) (3.0, 3.0) -. 2.828 < 0.001);
  assert(distance (2.0, 3.5) (0.2, 4.5) -. 2.059 < 0.001);
  assert(distance (-1.0, -1.0) (0.0, 0.0) -. 1.414 < 0.001);
  assert(perimeter ((1.0, 1.0), (3.0, 1.0), (3.0, 3.0)) -. 6.828 < 0.001);
  assert(perimeter ((1.0, 1.0), (3.0, 2.0), (2.0, 3.0)) -. 5.886 < 0.001)
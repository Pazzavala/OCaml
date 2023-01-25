open Char

let add_to (inc: int) (lst: int list) : int list = List.map ( (+) inc) lst

let exclaimify (strs: string list) : string list =
  let f str = str ^ "!"
  in
  List.map f strs


let any (bools: bool list) : bool =
  let f result elem =  (result || elem)
  in
  List.fold_left f false bools

let remove_odds (nums: int list) : int list =
  List.filter (fun x -> x mod 2 = 0) nums

let remove_caps (strLst: string list) : string list =
  let f str =  if code (String.get str 0) <= 90 then false else true
  in
  List.filter f strLst 


let _ =
  assert(add_to 3 [3;6;9] = [6;9;12]);
  assert(add_to 2 [] = []);
  assert(exclaimify ["hello"; "world"] = ["hello!"; "world!"]);
  assert(any [false; false; true]);
  assert(not (any [false; false; false]));
  assert(remove_odds [2;3;4;7;8] = [2;4;8]);
  assert(remove_caps ["Hello"; "hello"; "there"; "There"] = ["hello"; "there"])
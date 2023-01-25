module UtilityM = struct

  let prefixes_suffixes (lst: 'a list) : ('a list * 'a list) list  =
    let f (ans, pre, suf) word =
      match suf with
      | [] -> (ans, pre, suf)
      | x :: xs -> ([(pre, suf)] @ ans, pre @ [x], xs)
    in
    match List.fold_left f ([([],[])], [], lst) lst with
    | (ans, pre, suf) -> match (List.rev ans) @ [(pre, suf)] with
                          | ([],[]) :: xs -> xs
                          | _ -> ans

  let string_of_list (to_s: 'a -> string) (lst: 'a list) : string =
    let all_s elem acc = (to_s elem) ^ "\n" ^ acc
    in
    List.fold_right all_s lst ""

  let string_of_pair (a_to_s: 'a -> string) (b_to_s: 'b -> string) ((elemA, elemB): ('a * 'b)) : string =
    (a_to_s elemA) ^ ", " ^ (b_to_s elemB)

  let rec print_answers (ans: (string * string) list) =
    let id s = s in
    print_endline (string_of_list (string_of_pair id id) ans)

  let implode (cs: char list) : string =
    String.concat "" (List.map  (String.make 1) cs)

  let explode (s: string) : char list =
    let l = String.length s in
    let rec f i = if i = l then [] else s.[i] :: f (i+1) in
    f 0

  let read_words (file_name: string) : string list =
  let ic = open_in file_name in
  let rec read_lines ic = try
      let next_line = input_line ic in
      next_line :: read_lines ic
    with _ -> []
  in
  let raw_strings = read_lines ic 
  in
  List.filter (fun s -> String.length s > 0)
    (List.map String.trim raw_strings)
    
end
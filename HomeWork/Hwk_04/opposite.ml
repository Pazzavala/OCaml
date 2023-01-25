open Utility
open UtilityM

module OppositeF (S : Set.SetS) = struct
  
  let try_vowel (pos: (char list * char list)) (vowel: char) (dictionary: string S.t) : (string * string) list =
      let real = ['a';'e';'i';'o';'u'] in
      match pos with
      | (pref, x::xs) when not (List.mem x real) -> 
      (let created_word = implode (pref @ (vowel :: xs))
      in
      let word = implode (pref @ (x :: xs))
      in  
      if (S.elem created_word dictionary) && (word <> created_word)
      then [(word, created_word)]
      else [])
      | (_,_) -> []


  let try_position (pos: (char list * char list)) (set: string S.t) : (string * string) list =
    let vowels = ['a'; 'i'; 'e'; 'o'; 'u'; 'y']
  in
  List.fold_right (fun vowel accum -> (try_vowel pos vowel set) @ accum) vowels []


  let try_word (word: string) (set: string S.t) : (string * string) list =
    let all_positions = prefixes_suffixes (explode word)
  in
  List.fold_right (fun pos accum -> (try_position pos set) @ accum) all_positions []
                                                             
  let opposites  (dictionary: string list) : (string * string) list =
    let lst = List.filter (fun word -> String.length word = 6) dictionary
    in
    let rec build (lst: string list) (stack: string S.t): string S.t =
      match lst with
      | [] -> stack
      | x :: xs -> build xs (S.insert x stack)
    in let set = build lst S.empty
  in 
    List.fold_right (fun word accum -> (try_word word set) @ accum) lst []

end
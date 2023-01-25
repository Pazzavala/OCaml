open Solution
open Utility

module M = Opposites_List_Set

let _ =
  let args : string array = Sys.argv in
  if Array.length args < 2 
  then 
    print_endline "usage: `opposites_list_set.byte/native <word-bank-file>`"
  else
    let word_list : string list = Utility.UtilityM.read_words args.(1)
    in
    let answers : (string * string) list =  M.opposites word_list
    in
    Utility.UtilityM.print_answers (List.sort compare answers)
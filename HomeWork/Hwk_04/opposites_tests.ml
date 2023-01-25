open Tests
open Solution

module type Opposites_SolutionS = sig
  val opposites : string list -> (string * string) list
end

module Opposites_Tests (S: Opposites_SolutionS) = struct

let sunday : string list = 
  [ "defect"; "biking"; "strong"; "simple"; "salmon"; "cloudy"; "zither";
    "stanch"; "stance"; "defeat"; "bikini"; "clouds"; "sarong"; "gentle";
    "simile"; "saloon"; "either"; "stance"; "seance"; "gently" 
  ]
                           
let some_tests = [

    eval_test (fun () ->
        List.mem ("defect", "defeat") (S.opposites sunday), 
        true)
       "List.mem (\"defect\", \"defeat\") (S.opposites sunday)"
        Bool.to_string;

    eval_test (fun () ->
        List.mem ("simple", "simile") (S.opposites sunday), 
        true)
       "List.mem (\"simple\", \"simile\") (S.opposites sunday)"
        Bool.to_string;

    eval_test (fun () ->
        List.mem ("gently", "gentle") (S.opposites sunday), 
        true)
       "List.mem (\"gently\", \"gentle\") (S.opposites sunday)"
        Bool.to_string;

    eval_test (fun () ->
        List.length (S.opposites sunday), 11)
       "List.length (S.opposites sunday"
        Int.to_string

    ]

  let run () = run_tests some_tests

end

module Tests_List_Set = Opposites_Tests ( Opposites_List_Set )

module Tests_Tree_Set = Opposites_Tests ( Opposites_Tree_Set )

let _ =
  print_endline "Running tests for list set solution.";
  print_endline "------------------------------------";
  Tests_List_Set.run ();

  print_endline "\n\n";
  print_endline "Running tests for tree set solution.";
  print_endline "------------------------------------";
  Tests_Tree_Set.run ()


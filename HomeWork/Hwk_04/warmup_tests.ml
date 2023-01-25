open Tests
open Warmup


let ls1 : int LS.t = List.fold_right LS.insert [3;1;2] LS.empty

let ts1 : int TS.t = List.fold_right TS.insert [3;1;2] TS.empty

let some_tests = [
    (* Tests for prefixes_suffixes *)
    eval_test (fun () ->
        List.mem ([],[1;2;3]) (prefixes_suffixes [1;2;3]),
        true)
       "List.mem ([],[1;2;3]) (prefixes_suffixes [1;2;3])"
        Bool.to_string;

    eval_test (fun () ->
        List.mem ([1],[2;3]) (prefixes_suffixes [1;2;3]),
        true)
       "List.mem ([1],[2;3]) (prefixes_suffixes [1;2;3])"
        Bool.to_string;

    eval_test (fun () ->
        List.length (prefixes_suffixes [1;2;3]),
        4)
       "List.length (prefixes_suffixes [1;2;3])"
        Int.to_string;

    (* Tests for string_of_list *)
    eval_test (fun () ->
        string_of_list string_of_int [1;2;3],
        "1\n2\n3\n")
       "string_of_list string_of_int [1;2;3]"
        (fun s -> s);

    eval_test (fun () ->
        string_of_pair string_of_int string_of_bool (4, false),
        "4, false")
       "string_of_pair string_of_int string_of_bool (4, false)"
        (fun s -> s);

    eval_test (fun () ->
        string_of_list (string_of_pair string_of_int string_of_bool) [(4, false); (2, true)],
        "4, false\n2, true\n")
       "string_of_list (string_of_pair string_of_int string_of_bool) [(4, false); (2, true)],"
        (fun s -> s);

    eval_test (fun () ->
        LS.elem 4 (LS.insert 4 ls1),
        true)
       "LS.elem 4 (LS.insert 4 ls1)"
        Bool.to_string;

    eval_test (fun () ->
        TS.elem 4 (TS.insert 4 ts1),
        true)
       "TS.elem 4 (TS.insert 4 ts1)"
        Bool.to_string        
    ]

let () = run_tests some_tests




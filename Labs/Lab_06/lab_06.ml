let partition_compare (comp: 'a -> int) (lst: 'a list) : ('a list * 'a list * 'a list) =
  let f elem (buckets: 'a list * 'a list * 'a list) =
    match buckets with
    | (lst1, lst2, lst3) -> match comp elem with
                            | -1 -> (elem :: lst1, lst2, lst3)
                            | 0 -> (lst1, elem :: lst2, lst3) 
                            | 1 -> (lst1, lst2, elem :: lst3)
                            | _ -> raise (Failure "idk")
  in
  List.fold_right f lst ([], [], [])

let inc x = x + 1

let fold_map (f: 'a -> 'b) (alst: 'a list) : 'b list =
  let f elem blst = f elem :: blst
  in
  List.fold_right f alst []

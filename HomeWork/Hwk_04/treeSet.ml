module TreeSetM : Set.SetS = struct
  type 'a t = Empty
             | Fork of 'a t * 'a * 'a t

  let empty = Empty

  let rec insert (n: 'a)  (tree: 'a t): 'a t = 
    match tree with
    | Empty -> Fork(Empty, n, Empty)
    | Fork(lt, v, rt) -> match compare v n with
                         | -1 -> Fork(lt, v, insert n rt)
                         | 0 -> Fork(lt, v, rt)
                         | 1 -> Fork(insert n lt, v, rt) 
                         | _ -> raise (Failure "unexpected")

  let rec elem (n: 'a) (t: 'a t) : bool = 
    match t with
    | Empty -> false
    | Fork (lt, v, rt) -> match compare v n with
                          | -1 -> elem n rt
                          | 0 -> true
                          | 1 -> elem n lt
                          | _ ->  raise (Failure "unexpected")

end
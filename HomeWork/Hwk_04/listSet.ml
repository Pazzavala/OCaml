module ListSetM : Set.SetS = struct
  type 'a t = 'a list

  let empty = []

  let insert (v: 'a) (lst: 'a t) : 'a t = v :: lst

  let elem (v: 'a) (lst: 'a t) : bool =
    let in_list v' acc = v = v' || acc
  in
  List.fold_right in_list lst false

end 
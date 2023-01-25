type coordinate = int * int

let goal c = (c = (3,5)) || (c = (5,1)) 

let maze_moves (position: coordinate) : coordinate list =
  match position with
  | (1,1) -> [(2,1)]
  | (1,2) -> [(1,3); (2,2)]
  | (1,3) -> [(1,2); (1,4); (2,3)]
  | (1,4) -> [(1,3); (1,5)]
  | (1,5) -> [(1,4); (2,5)]
  | (2,1) -> [(1,1); (3,1)]
  | (2,2) -> [(1,2); (3,2)]
  | (2,3) -> [(1,3)]
  | (2,4) -> [(2,5); (3,4)]
  | (2,5) -> [(2,4);(1,5)]
  | (3,1) -> [(3,2); (2,1)]
  | (3,2) -> [(3,1); (3,3); (2,2)]
  | (3,3) -> [(3,2); (3,4); (4,3)]
  | (3,4) -> [(3,3); (2,4); (4,4)]
  | (3,5) -> [(4,5)]
  | (4,1) -> [(4,2)]
  | (4,2) -> [(4,1); (3,2)]
  | (4,3) -> [(3,3); (5,3)]
  | (4,4) -> [(4,5); (3,4)]
  | (4,5) -> [(4,4); (3,5); (5,5)]
  | (5,1) -> [(5,2)]
  | (5,2) -> [(5,1); (5,3)]
  | (5,3) -> [(5,2); (5,4); (4, 3)]
  | (5,4) -> [(5,3)]
  | (5,5) -> [(4,5)]
  | (_,_) -> raise (Failure "IDK")

exception FoundPath of coordinate list

let maze () : coordinate list option =
  let rec go_from (pos: coordinate) (path: coordinate list) =
    if goal pos
      then raise (FoundPath path)
    else
      let positions = List.filter (fun pos -> not (List.mem pos path)) (maze_moves pos)
    in
    let rec try_positions (positions: coordinate list) : unit =  
      match positions with
      | [] -> ()
      | pos :: poss -> go_from pos (path @ [pos]);
                       try_positions poss
    in try_positions positions
  in try go_from (2,3) [(2,3)]; None with
  | FoundPath p -> Some p
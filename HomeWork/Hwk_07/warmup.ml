type formula = Bool of bool (* Boolean literals of true and false *)
            | And  of formula * formula
            | Implies of formula * formula
            | Or of formula * formula
            | Not of formula 
            | Prop of string (* variables that are assigned Boolean values of true or false *)
          
and subst = (string * bool) list

exception UnboundVariable of string

(* Function used in class - Van Wyk *)
let rec lookup (x: string) (subset: subst): bool =
  match subset with
  | [] -> raise (UnboundVariable x)
  | (name,boolean):: rest when x = name -> boolean
  | _ :: rest -> lookup x rest

let rm_duplicates (lst: 'a list) : 'a list =
  let collect_unique elem to_keep =
    if List.mem elem to_keep then to_keep else elem::to_keep
  in List.fold_right collect_unique lst []

let rec eval (f: formula) (subset: subst) : bool =
  match f with
  | Bool b -> b
  | Not (f) -> (match eval f subset with
              | b -> not b) 
  | And (f1, f2) -> (match eval f1 subset, eval f2 subset with
                    | b1, b2 -> b1 && b2)
  | Or (f1, f2) -> (match eval f1 subset, eval f2 subset with
                  | b1, b2 -> b1 || b2)
  | Implies (f1, f2) -> (match eval f1 subset, eval f2 subset with
                        | b1, b2 -> not b1 || b2)
  | Prop (x) -> lookup x subset

let freevars (f: formula) : string list =
  let rec build f = 
    match f with
    | Bool b -> []
    | Prop x -> [x]
    | Not (f) -> build f
    | And (f1, f2) | Or (f1, f2) | Implies (f1, f2) -> build f1 @ build f2
  in rm_duplicates (build f)

  
(* ****************** My work ******************* *)

let tautology_checker (f:formula) : subst option =
  let rec try_subset (partial_subset: subst) (rest: string list) =
    if rest = [] && partial_subset <> [] && eval f partial_subset = false
      then Some partial_subset
    else
      match rest with
      | [] -> None
      | x :: xs -> match try_subset ((x, true)::partial_subset) xs with
                  | Some set -> Some set
                  | None -> try_subset ((x, false) :: partial_subset) xs 
in try_subset [] (freevars f)

let t1 = Or (Prop "P", Not (Prop "P"))
let t2 = Or (Implies (Prop "P", Prop "Q"), Implies (Prop "Q", Prop "P"))
let t3 = Or (Or ( Not (Prop "P"), Prop "Q"), Or ( Not (Prop "Q"), Prop "P"))
let nt1 = Or (Prop "P", Prop "Q")
let nt2 = And (Prop "P", Prop "Q")

let _ =
  assert(tautology_checker t1 = None);
  assert(tautology_checker t2 = None);
  assert(tautology_checker t3 = None);
  assert(tautology_checker nt1 <> None);
  assert (tautology_checker nt1 = Some [ ("Q", false); ("P", false) ] || 
  tautology_checker nt1 = Some [ ("P", false); ("Q", false) ]);
  assert(tautology_checker nt2 <> None);

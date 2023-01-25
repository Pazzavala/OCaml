(* Hwk 06 Warmup. 

   Extend the functions below to match all of the constructs
   in the type `expr`.

   Some of the parts to be filled in can be found in the 
   functions we developed in class in the `Expressions`
   directory.  If you use work from class please add a note
   to identify the work that is not originally yours.
 *)

type value 
  = Int of int
  | Bool of bool

type environment = (string * value) list
                               
and expr 
  = Val of value

  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Neg of expr 

  | Lt  of expr * expr
  | Gt  of expr * expr
  | Eq  of expr * expr
  | And of expr * expr
  | Or  of expr * expr
  | Not of expr

  | Let of string * expr * expr
  | Id  of string


exception DivisionByZero of value
exception UnboundVariable of string
exception IncorrectType of string


(* Function used in class - Van Wyk *)
let rec lookup (x: string) (env: environment): value =
  match env with
  | [] -> raise (UnboundVariable x)
  | (name,value):: rest when x = name -> value
  | _ :: rest -> lookup x rest


let rec eval (e:expr) (env: environment) : value =
  match e with
  | Val v -> v
  | Id x -> lookup x env
  | Add (e1, e2) ->
     ( match eval e1 env, eval e2 env with
       | Int v1, Int v2 -> Int (v1 + v2)
       | _ -> raise (IncorrectType "Add")
     )
  | Sub (e1, e2) ->
    ( match eval e1 env, eval e2 env with
      | Int v1, Int v2 -> Int (v1 - v2)
      | _ -> raise (IncorrectType "Sub")
    )
  | Mul (e1, e2) ->
    ( match eval e1 env, eval e2 env with
      | Int v1, Int v2 -> Int (v1 * v2)
      | _ -> raise (IncorrectType "Mul")
    )
  | Div (e1, e2) ->
    ( match eval e1 env, eval e2 env with
      | Int v1, Int 0 -> raise (DivisionByZero (Int v1))
      | Int v1, Int v2 -> Int (v1 / v2)
      | _ -> raise (IncorrectType "Div")
    )
  | Neg (e1) ->
    ( match eval e1 env with
      | Int v1 -> Int (-v1)
      | _ -> raise (IncorrectType "Neg")
    )
  | Lt (e1, e2) ->
    ( match eval e1 env, eval e2 env with
      | Int v1, Int v2 -> Bool (v1 < v2)
      | _ -> raise (IncorrectType "Lt")
    )
  | Gt (e1, e2) ->
    ( match eval e1 env, eval e2 env with
      | Int v1, Int v2 -> Bool (v1 > v2)
      | _ -> raise (IncorrectType "Gt")
    )
  | Eq (e1, e2) ->
    ( match eval e1 env, eval e2 env with
      | Int v1, Int v2 -> Bool (v1 = v2)
      | _ -> raise (IncorrectType "Eq")
    )
  | And (e1, e2) ->
    ( match eval e1 env, eval e2 env with
      | Bool v1, Bool v2 -> Bool (v1 && v2)
      | _ -> raise (IncorrectType "And")
    )
  | Or (e1, e2) ->
    ( match eval e1 env, eval e2 env with
      | Bool v1, Bool v2 -> Bool (v1 || v2)
      | _ -> raise (IncorrectType "Or")
    )
  | Not (e1) ->
    ( match eval e1 env with
      | Bool v1 -> Bool (not v1)
      | _ -> raise (IncorrectType "Not")
    )
  | Let (x, e1, e2) ->
    ( match eval e1 env with
    | Int v1 -> eval e2 ((x, Int v1) :: env)
    | Bool b1 -> eval e2 ((x, Bool b1) :: env)
    )
  


(* Some sample expressions *)

(* Arithmetic *)
let e01 = Add (Val (Int 3), Val (Int 5))
let e02 = Mul (Add (Val (Int 3), Val (Int 2)), 
               Sub (Val (Int 4), Val (Int 2)))
let e03 = Sub (Val (Int 8), Val (Int 2))
let e04 = Add (Val (Int 8), Neg (Val (Int 2)))


(* Relational and logical operations *)
let e05 = Lt (Add (Val (Int 3), Val (Int 5)), Val (Int 10))

let e06 = And (Lt (Val (Int 10), Val (Int 15)),
              Not (Eq (Val (Int 10), Val (Int 8))))

let e07 = Or (Gt (Val (Int 10), Val (Int 15)),
              Not (Eq (Val (Int 10), Val (Int 8))))



(* Let expressions *)
let e08 = Let ("x", Add (Val (Int 3), Val (Int 4)),
              Add (Id "x", Val (Int 5))
           )

   
let e09 = Let ("x", Add (Val (Int 3), Val (Int 4)),
              Lt (Id "x", Val (Int 5))
           )
       
(* ``let x = 3 < 5 in x && let x = 1 + 2 in x = 3 *)
let e10 = Let ("x",
              Lt (Val (Int 3), Val (Int 5)),
              And (Id "x",
                   Let ("x",
                        Add (Val (Int 1), Val (Int 2)),
                        Eq (Id "x", Val (Int 3))
                       )
                  )
             )


(* Errors *)

(* Unbound Variables *)
let e20 = And (Id "y", Val (Bool true))


(* ``let x = 3 < 5 in y && let x = 1 + 2 in x = 3 *)
let e21 = Let ("x",
              Lt (Val (Int 3), Val (Int 5)),
              And (Id "y",
                   Let ("x",
                        Add (Val (Int 1), Val (Int 2)),
                        Eq (Id "x", Val (Int 3))
                       )
                  )
             )

(* let x = x + 4 in y && true *)
let e22 = Let ("x", Add (Id "x", Val (Int 4)),
                 And (Id "y", Val (Bool true))
              )


(* Division by Zero *)

let e23 = Let ("x", Val (Int 0), Div (Val (Int 4), Id "x"))


(* Type Errors *)
let e24 = Add (Val (Int 3), Val (Bool true))

let e25 = Let ("x", Add (Val (Int 3), Val (Int 4)),
                 And (Id "x", Val (Bool true))
              )





let _ =
  assert (eval e01 [] = Int 8);
  assert (eval e02 [] = Int 10);
  assert (eval e03 [] = Int 6);
  assert (eval e04 [] = Int 6);

  assert (eval e05 [] = Bool true);
  assert (eval e06 [] = Bool true);
  assert (eval e07 [] = Bool true);

  assert (eval e08 [] = Int 12);
  assert (eval e09 [] = Bool false);
  assert (eval e10 [] = Bool true)


let _ = 

  assert (try (fun _ -> false) (eval e20 []) with
          | UnboundVariable "y" -> true
          | _ -> false);
  assert (try (fun _ -> false) (eval e21 []) with
          | UnboundVariable "y" -> true
          | _ -> false);
  assert (try (fun _ -> false) (eval e22 []) with
          | UnboundVariable v -> (v = "x") || (v = "y")
          | _ -> false)

let _ =
  assert (try (fun _ -> false) (eval e23 []) with
          | DivisionByZero (Int 4) -> true
          | _ -> false)

let _ = 
  assert (try (fun _ -> false) (eval e24 []) with
          | IncorrectType "Add" -> true
          | _ -> false);

  assert (try (fun _ -> false) (eval e25 []) with
          | IncorrectType "And" -> true
          | _ -> false)

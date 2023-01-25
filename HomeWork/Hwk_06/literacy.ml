(* Hwk 06 Literacy. 

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
  | Closure of string * expr * environment

and environment = (string * value) list
                               
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

  | Lam of string * expr
  | App of expr * expr


(* Part 1: free variables *)

let rec freevars (e: expr) : string list =
  match e with
  | Val _ -> []
  | Id x -> [x]
  | Neg (e1) | Not (e1) -> freevars e1
  | Add (e1, e2) | Sub (e1, e2) | Mul (e1, e2) | Div (e1, e2) | Lt (e1, e2) | Gt (e1, e2) 
  | Eq (e1, e2) | And (e1, e2) | Or (e1, e2) | App (e1, e2) -> freevars e1 @ freevars e2
  | Lam (f, e1) -> List.filter (fun f' -> f <> f') (freevars e1)
  | Let (x, e1, e2) -> freevars e1 @
                       (List.filter (fun x' -> x <> x') (freevars e2))
  



(* Part 2: evaluation *)
exception DivisionByZero of value
exception UnboundVariable of string
exception IncorrectType of string


(* Function used in class - Van Wyk *)
let rec lookup (x: string) (env: environment) : value =
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
      | Bool b1, Bool b2 -> Bool (b1 && b2)
      | _ -> raise (IncorrectType "And")
    )
  | Or (e1, e2) ->
    ( match eval e1 env, eval e2 env with
      | Bool b1, Bool b2 -> Bool (b1 || b2)
      | _ -> raise (IncorrectType "Or")
    )
  | Not (e1) ->
    ( match eval e1 env with
      | Bool b -> Bool (not b)
      | _ -> raise (IncorrectType "Not")
    )
  | Let (x, e1, e2) ->
    ( match eval e1 env with
    | Int v -> eval e2 ((x, Int v) :: env)
    | Bool b -> eval e2 ((x, Bool b) :: env)
    | Closure (y, body, envC) -> eval e2 ((x, Closure (y, body, envC)) :: env)
    )
  | App (e1, e2) -> 
    (match eval e1 env, eval e2 env with
    | Closure (y, body, envC), Int v -> eval body ((y, Int v) :: envC)
    | Closure (y, body, envC), Bool b -> eval body ((y, Bool b) :: envC)
    | _ -> raise (IncorrectType "App")
    ) 
  | Lam (y, body) -> 
    let vars = freevars (Lam (y, body)) 
      in let envC = (List.map (fun x -> (x, lookup x env)) vars)
          in Closure (y, body, envC)

 
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
(* let x = 3 + 4 in x + 5 *)
(* let x = 7 in x + 5 *)
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


(* Functions *)
let e11 = Let ("inc", Lam ("n", Add (Id "n", Val (Int 1))),
               App (Id "inc", Val (Int 2)))

let e12 = Lam ("n", Add (Id "n", Val (Int 1)))
(* assert (eval e12 [] = Closure ("n", Add (Id "n", Val (Int 1)), [])); *)

let e13 = Let ("add", 
               Lam ("x", Lam ("y", Add (Id "x", Id "y"))),
               App (App (Id "add", Val (Int 5)), Val (Int 3))
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
              
(* Unbound Variables *)
let e26 = Lam ("y", Add (Id "x", Id "y"))

(* Type Errors *)
let e27 = Let ("inc", Val (Int 1),
               App (Id "inc", Val (Int 2)))


let _ =
  assert (List.sort compare (freevars e01) = []);
  assert (List.sort compare (freevars e08) = []);
  assert (List.sort compare (freevars e20) = ["y"]);
  assert (List.sort compare (freevars e21) = ["y"]);
  assert (List.sort compare (freevars e22) = ["x"; "y"]);


  assert (List.sort compare (freevars e11) = []);
  assert (List.sort compare (freevars e12) = []);
  assert (List.sort compare (freevars e13) = []);
  assert (List.sort compare (freevars e26) = ["x"])



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
  assert (eval e10 [] = Bool true);


  assert (eval e11 [] = Int 3);
  assert (eval e12 [] = Closure ("n", Add (Id "n", Val (Int 1)), []));
  assert (eval e13 [] = Int 8);

  assert (eval e26 [("x", Int 3)] = 
            Closure ("y", Add (Id "x", Id "y"), [("x", Int 3)]))


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
          | _ -> false);


  assert (try (fun _ -> false) (eval e27 []) with
          | IncorrectType "App" -> true
          | _ -> false)


let _ =
  assert (try (fun _ -> false) (eval e26 []) with
          | UnboundVariable "x" -> true
          | _ -> false)
       (* This happens since we need to create a closure envirnoment
          that include "x" and its value - but "x" is not declared. *)

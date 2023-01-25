# Homework 5: Reasoning about Correctness.

by Maria Zavala

## Warm-Up

### Problem 1: A property for `prod`

```ocaml
let rec prod (lst: int list) : int = 
  match lst with
  | [] -> 1
  | y::ys -> y * prod ys
```

 show by induction that for any two lists `l1` and `l2` of type `int list` that:

```ocaml
P(L1, L2): prod (l1 @ l2) = prod l1 * prod l2
P(L1): for all L2, prod (l1 @ l2) = prod l1 * prod l2
```

##### 1. Principle of Induction

```ocaml
for all lst, P(lst) holds if
	- P([]) holds
	- P(xs) implies that P(x :: xs) holds, for any x
```

##### 2. Base case to Prove

```ocaml
P([], L2): prod ([] @ l2) = prod [] * prod l2
```

##### 3. Proof of the Base Case

```ocaml
	prod ([] @ l2) 
= prod L2 - by properties of list and @
= 1 * prod L2 - by properties of multiplication
= prod [] * prod l2 - by definition of prod
```

##### 4. Inductive case to prove

```ocaml
P(x :: xs): for all L2, prod (x :: xs @ l2) = prod (x :: xs) * prod l2
```

##### 5. Inductive hypothesis

```ocaml
Show: prod (x :: xs @ l2) = prod (x :: xs) * prod l2
Given: prod (xs @ l2) = prod xs * prod l2
```

##### 6. Inductive case

```ocaml
	prod (x :: xs @ l2) 
= prod (x :: (xs @ l2)) - by properties of list and @
= x * prod (xs @ l2) - by definition of prod
= x * prod xs * prod l2 - by inductive hypothesis
= prod (x :: xs) * prod l2 - by definition of prod
```

### Problem 2: sum 2 ways

```ocaml
let rec sum (lst: int list) : int = 
  match lst with
  | [] -> 0
  | x::xs -> x + sum xs

let rec foldr (f: 'a -> 'b -> 'b) (lst: 'a list) (base: 'b) : 'b =
  match lst with
  | [] -> base
  | x::xs -> f x (foldr f xs base)

let sum_fold (lst: int list) : int = foldr ( + ) lst 0
```

Using the definitions above show that the following holds for any list `lst` of type `int list`:

```ocaml
  sum lst = sum_fold lst
```

##### 1. Principle of Induction

```ocaml
for all lst, P(lst) holds if
	- P([]) holds
	- P(xs) implies that P(x :: xs) holds, for any x
```

##### 2. Base case to Prove

```ocaml
P([]): sum [] = sum_fold []
```

##### 3. Proof of the Base Case

```ocaml
	sum [] 
= 0 - by definition of sum
= foldr ( + ) [] 0 - by def - by definion of foldr
= sum_fold [] - by definion of sum_fold
```

##### 4. Inductive case to prove

```ocaml
P(x :: xs): sum (x :: xs) = sum_fold (x :: xs)
```

##### 5. Inductive hypothesis

```ocaml
Show: sum (x :: xs) = sum_fold (x :: xs)
Given: sum xs = sum_fold xs
```

##### 6. Inductive case

```ocaml
	sum (x :: xs) 
= x + sum xs - by the definition of sum
= x + sum_fold xs - by inductive hypothesis
= x + foldr (+) xs 0 -  by definition of sum_fold
= (+) x (foldr (+) xs 0) - by rearrangement
= sum_fold (x :: xs) - by definition of sum_fold
```

### Problem 3: Maximum of natural numbers

```ocaml
type nat = Zero | Succ of nat

let rec maximum (lst: nat list) : nat = 
match lst with
  | [] -> Zero 
  | n::ns -> maxnat n (maximum ns)
```

One property that you can use is that `Zero` is the smallest value of type `nat`. That is, for any `n : nat`

````ocaml
maxnat Zero n = n
maxnat n Zero = n
````

We also have an associative law for `maxnat`. That is:

```ocaml
maxnat a (maxnat b c) = maxnat (maxnat a b) c
```

Show that for any two lists `l1` and `l2` of type `nat list`:

```ocaml
P(l1, l2): maximum (l1 @ l2) = maxnat (maximum l1) (maximum l2)
P(l1): for all l2,  maximum (l1 @ l2) = maxnat (maximum l1) (maximum l2)
```

##### 1. Principle of Induction

```ocaml
for all lst, P(lst) holds if
	- P([]) holds
	- P(xs) implies that P(x :: xs) holds
```

##### 2. Base case to Prove

```ocaml
P([]): maximum ([] @ l2) = maxnat (maximum []) (maximum l2)
```

##### 3. Proof of the Base Case

```ocaml
	maximum ([] @ l2) 
= maximum l2 - by properties of [] and @
= maxnat Zero (maximum l2) - by property 'Zero' is smallest value of type 'nat'
= maxnat (maximum []) (maximum l2) - by definition of maximum
```

##### 4. Inductive case to prove

```ocaml
P(x :: xs): maximum ((x :: xs) @ l2) = maxnat (maximum (x :: xs)) (maximum l2)
```

##### 5. Inductive hypothesis

```ocaml
Show: maximum (x :: xs @ l2) = maxnat (maximum x :: xs) (maximum l2)
Given: maximum (xs @ l2) = maxnat (maximum xs) (maximum l2)
```

##### 6. Inductive case

```ocaml
	maximum (x :: xs @ l2) 
= maximum (x :: (xs @ l2)) - by properties of list and @
= maxnat x (maximum (xs @ l2)) - by definiton of maximum
= maxnat x (maxnat (maximum xs) (maximum l2)) - by inductive hypothesis
= maxnat (maxnat x (maximum xs)) (maximum l2) - by associative law
= maxnat (maximum x :: xs) (maximum l2) - by definition of maximum
```

## Literacy

### Problem 4: Principles of induction

```ocaml
type 'a tree = Empty
             | Node of 'a tree * 'a * 'a tree
```

##### Principle of Induction for type `'a tree`

```ocaml
for all t : 'a tree, P(t) holds if
	- P(Empty), and
	- P(t1), and P(t2) implies P(Node(t1, v, t2) (for any value v of type 'a)
```

#### Part 1. What is principle of induction for the following type?

```ocaml
type 'a tree = Leaf 
  			 | Fork of 'a tree * 'a * 'a tree
```

##### Principle of Induction for type `'a tree`

```ocaml
for all t : 'a tree, P(t) hold if
	- P(Leaf) holds, and
	- P(lt), and P(rt) implies that P(Fork(lt, v, rt) (for any value v of type 'a))
```

#### Part 3. What is principle of induction for the following type?

```ocaml
type 'a rose_tree = Rose of 'a * 'a rose_tree list
```

##### Principle of Induction for type `'a rose_tree`

```ocaml
for all r : 'a rose_tree, P(r) hold if
	- P(Rose v []) holds, and
	- P(rts) implies that P(Rose (v, (rt :: rts))) holds, (for any value v of type 'a)
```

#### Part 3. What is principle of induction for the following type?

```ocaml
type 'a zom_tree = Zero
                 | One of 'a
                 | Many of 'a * 'a zom_tree * 'a zom_tree
```

##### Principle of Induction for type `'a zom_tree`

```ocaml
for all z : 'a zom_tree, P(z) holds if
	- P(Zero) holds
	- P(One v) holds, for any v
	- P(z1), and P(z2) implies that P(Many(v, z1, z2)) holds, (for any value v of type 'a)
```

### Problem 5: Appending non-empty lists

```ocaml
type 'a nelist = One of 'a
               | Cons of 'a * 'a nelist
```

Below is a function that appends 2 `'a nelist` values to create another with all of their elements. It is analogous to the `@` append operator over `'a list` values in OCaml.

```ocaml
let rec app (l1: 'a nelist) (l2: 'a nelist) : 'a nelist =
  match l1 with
  | One v -> Cons (v, l2)
  | Cons (v,vs) -> Cons (v, app vs l2)
```

Using these definitions above show that the following associativity property of `nelist`s holds for any `'a nelist` values `l1`, `l2`, and `l3`.

```ocaml
app (app l1 l2) l3 = app l1 (app l2 l3)
```

##### 1. Principle of Induction for type `'a nelist`

```ocaml
for all nL : 'a nelist, P(nL) holds if
	- P(One v) holds
	- P(tL) implies that P(Cons(v, tL)) holds, for any v of type 'a
```

##### 2. Base case to Prove

```ocaml
P(One v): app (app (One v) l2) l3 = app (One v) (app l2 l3)
```

##### 3. Proof of the Base Case

```ocaml
	app (app (One v) l2) l3 
= app (Cons (v, l2)) l3 - by definition of app
= Cons (v, app l2 l3) - by defintion of app
= app (One v) (app l2 l3) - by defintion of app
```

##### 4. Inductive case to prove

```ocaml
P(Cons(v, tL)): app (app Cons(v, tL) l2) l3 = app Cons(v, tL) (app l2 l3)
```

##### 5. Inductive hypothesis

```ocaml
Show: app (app Cons(v, tL) l2) l3 = app Cons(v, tL) (app l2 l3)
Given: app (app tL l2) l3 = app tL (app l2 l3)
```

##### 6. Inductive case

```ocaml
	app (app Cons(v, tL) l2) l3 
= app (Cons (v, app tL l2)) l3 - by definition of app
= Cons (v, app (app tL l2) l3) - by definition of app
= Cons (v, app tL (app l2 l3)) - by Inductive Hypothesis
= app Cons(v, tL) (app l2 l3) - by definition of app
```

### Problem 6: Reversing non-empty lists

```ocaml
type 'a nelist = One of 'a
               | Cons of 'a * 'a nelist
```

Below is a function that appends 2 `'a nelist` values to create another with all of their elements. It is analogous to the `@` append operator over `'a list` values in OCaml.

```ocaml
let rec app (l1: 'a nelist) (l2: 'a nelist) : 'a nelist =
  match l1 with
  | One v -> Cons (v, l2)
  | Cons (v,vs) -> Cons (v, app vs l2)
```

Below is a function that reverses an `'a nelist` value:

```ocaml
let rec rev (lst: 'a nelist) : 'a nelist =
  match lst with
  | One v -> One v
  | Cons (h, t) -> app (rev t) (One h)
```

Using these definitions above show that the following associativity property of `nelist`s holds for any `'a nelist` values `l1` and `l2`.

```ocaml
rev (app l1 l2) = app (rev l2) (rev l1)
```

##### 1. Principle of Induction for type `'a nelist`

```ocaml
for all nL : 'a nelist, P(nL) holds if
	P(One v) holds for any v
	P(tl) implies that P(Cons (v, tl)) holds for any v of type 'a
```

##### 2. Base case to Prove

```ocaml
P(One v): rev (app (One v) l2) = app (rev l2) (rev (One v))
```

##### 3. Proof of the Base Case

```ocaml
	rev (app (One v) l2) 
= rev (Cons (v, l2)) - by definition of app
= app (rev l2) (One v) - by definition of rev
= app (rev l2) (rev (One v)) - by definition of rev
```

##### 4. Inductive case to prove

```ocaml
P(Cons(v, tl)): rev (app (Cons (v, tl)) l2) = app (rev l2) (rev (Cons(v, tl)))
```

##### 5. Inductive hypothesis

```ocaml
Show: rev (app (Cons (v, tl)) l2) = app (rev l2) (rev (Cons(v, tl)))
Given: rev (app (tl l2)) = app (rev l2) (rev tl)
```

##### 6. Inductive case

```ocaml
	rev (app (Cons(v, tl)) l2) 
= rev (Cons (v, app tl l2)) - by definition of app
= app (rev (app tl l2)) (One v) - by definition of rev
= app (app (rev l2) (rev tl)) (One v) - by Inductive Hypothesis
= app (rev l2) (app (rev tl) (One v)) - by Proof in problem 5
= app (rev l2) (rev (Cons(v, tl))) - by definition of rev
```

## Mastery

### Problem 7: Completing a proof from class

```ocaml
minTree t  = minNeList (flatten t)
```

in which `t` has type `'a tree`. The relevant code is shown below:

```ocaml
type 'a tree = Leaf of 'a
             | Fork of 'a tree * 'a * 'a tree

let rec minNEList (ns: 'a nelist) : 'a = 
  match ns with
  | One x -> x
  | Cons (x, xs) -> min x (minNEList xs)
  
let rec minTree (t: int tree) : int = 
  match t with
  | Leaf v -> v
  | Fork (l, v, r) -> min v (min (minTree l) (minTree r))

let rec app (l1: 'a nelist) (l2: 'a nelist) : 'a nelist =
  match l1 with
  | One v -> Cons (v, l2)
  | Cons (v,vs) -> Cons (v, app vs l2)
  
app (rev l2) (app (rev tl) (One v)) - by Proof in problem 5
app (app (One v) l2) l3 = app (One v) (app l2 l3)

let rec flatten (t: 'a tree) : 'a nelist = 
  match t with
  | Leaf v -> One v
  | Fork (l, v, r) -> app (flatten l) (app (One v) (flatten r))
```

##### 1. Principle of Induction

```ocaml
for all t : 'a tree, P(t) holds if
	P(Leaf v) holds
	P(t1) and P(t2) implies that P(Fork(t1, v, t2)) holds for any v of type 'a
```

##### 2. Base case to Prove

```ocaml
P(Leaf v): minTree Leaf v = minNelist (flatten (leaf v))
```

##### 3. Proof of the Base Case

```ocaml
	minTree Leaf v 
= v - by definition of minTree
= minNelist (One v) - by definition of minNelist
= minNelist (flatten (leaf v)) - by definition of flatten
```

##### 4. Inductive case to prove

```ocaml
P(Fork(t1, v, t2)): minTree (Fork (t1, v, t2)) = minNelist (flatten (Fork (t1, v, t2)))
```

##### 5. Inductive hypothesis

```ocaml
Show: minTree (Fork (t1, v, t2)) = minNelist (flatten (Fork (t1, v, t2)))
Given: minTree t1 = minNelist (flatten t1)
	   minTree t2 = minNelist (flatten t2)
```

##### 6. Inductive case

```ocaml
	minTree (Fork (t1, v, t2)) 
= min v (min  (minTree t1) (minTree t2)) - by definition of minTree
= min v (min (minNelist (flatten t1)) (minNelist (flatten t2))) - by Inductive Hypothesis
= min (minNelist (flatten t1)) (min  v (minNelist (flatten t2))) - by commutative property
= min (minNelist (flatten t1)) (minNEList (Cons (v, (flatten t2)))) - by definition of minNEList
= minNelist (app (flatten t1)) (Cons (v, (flatten t2))) - by Lemma 1 prove under
= minNelist (app (flatten t1) (app (One v) (flatten t2))) - by definition of app
= minNelist (flatten (Fork (t1, v, t2))) - by definition of flatten
```

#### Lemma 1
```ocaml
min (minNEList l1) (minNEList l2) = minNEList (app l1 l2)
```

##### 1. Principle of Induction for type `'a nelist`
```ocaml
for all nL : 'a nelist P(nL) hods if
  - P(One v) holds
  - P(tL) implies that P(Cons(v, tL)) holds, for any v of type 'a
```

##### 2. Base case to Prove
```ocaml
P(One v): min (minNEList (One v)) (minNEList l2) = minNEList (app (One v) l2)
```

##### 3. Proof of the Base Case
```ocaml
  min (minNEList (One v)) (minNEList l2)
= min ((v) minNEList l2) - by definition of minNEList
= minNEList (Cons (v, l2)) - by definition of minNEList
= minNEList (app (One v) l2) - by definition of app
```
##### 4. Inductive case to prove
```ocaml
P(Cons(v, tL)): min (minNEList (Cons(v, tL))) (minNEList l2) = minNEList (app (Cons(v, tL)) l2)
```
##### 5. Inductive hypothesis
```ocaml
Show: min (minNEList (Cons(v, tL))) (minNEList l2) = minNEList (app (Cons(v, tL)) l2)
Given: min (minNEList tL) (minNEList l2) = minNEList (app tL l2)
```

##### 6. Inductive case
```ocaml
  min (minNEList (Cons(v, tL))) (minNEList l2)
= min (min v (minNEList tL)) (minNEList l2) - by definition of minNEList
= min v (min (minNEList tL) (minNEList l2)) - by associative proprty
= min v (minNEList (app tL l2)) - by Inductive Hypothesis
= minNEList (Cons (v, app tL l2)) - by definition of minNEList
= minNEList (app (Cons(v, tL)) l2) - by definition of app
```


### Problem 8: A Property over your list-based sets from Hwk 4

```ocaml
  elem e (insert e s) = true
```

```ocaml
type 'a t = 'a list

let empty = []

let rec insert (e: 'a) (l: 'a list) : 'a list =
	match l with
	| [] -> [e]
	| x::xs when e < x -> e::x::xs
	| x::xs -> x :: (place e xs)
    
let rec elem (e: 'a) (l: 'a list) : bool =
 	match l with
 	| [] -> false
 	| x :: xs -> e = x || (e > x && is_elem e xs)
```

##### 1. Principle of Induction

```ocaml
for all lst: 'a list, P(lst) holds if
	P([]) holds
    P(xs) implies that P(x :: xs) holds for any x of type 'a
```

##### 2. Base case to Prove

```ocaml
P([]): elem e (insert e []) = true
```

##### 3. Proof of the Base Case

```ocaml
	elem e (insert e []) 
= elem e [e] - by definition of insert
= e = e - by definition of elem
= true - by boolean evaluation
```

##### 4. Inductive case to prove

```ocaml
P(x::xs): elem e (insert e (x::xs)) = true
```

##### 5. Inductive hypothesis

```ocaml
show: elem e (insert e (x::xs)) = true
given: elem e (insert e xs) = true
```

##### 6. Inductive case

```ocaml
(* Case: e < x *)
	elem e (insert e (x::xs)) 
= elem e (e::x::xs) - by definition of insert
= e = e - by definition of elem
= true

(* Case: e > x *)
	elem e (insert e (x::xs)) 
= elem e (x :: (insert e xs)) - by definition of insert
= e = x || (true && elem e (insert e xs)) - by definition of elem
= false || (true && true) - by Inductive Hypothesis and boolean evaluation
= true - by boolean evaluation

(* Case: e = x *)
	elem e (insert e (x::xs)) 
= elem e (x :: (insert e xs)) - by definition of insert
= e = x - by definition of elem
= true - by boolean evaluation of case: e = x
```

### Problem 9: A property over your tree-based sets from Hwk 4

### 

```ocaml
  elem e (insert e s) = true
```

```ocaml
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

```

##### 1. Principle of Induction

```ocaml
for all t : 'a tree, P(t) holds if
	P(Empty) holds
	P(t1) and P(t2) implies that P(Fork(t1, v, t2)) holds for any v of type 'a
```

##### 2. Base case to Prove

```ocaml
P(Empty): elem e (insert e Empty) = true
```

##### 3. Proof of the Base Case

```ocaml
	elem e (insert e Empty) 
= elem e (Fork(Empty, e, Empty)) - by definition of insert
= true - by definition of elem
```

##### 4. Inductive case to prove

```ocaml
P(Fork(t1, v, t2)): elem e (insert e Fork(t1, v, t2)) = true
```

##### 5. Inductive hypothesis

```ocaml
show: elem e (insert e Fork(t1, v, t2)) = true
given: elem e (insert e t1) = true
	   elem e (insert e t2) = true
```

##### 6. Inductive case

```ocaml
(* Case: e < v *)
	elem e (insert e Fork(t1, v, t2)) 
= elem e (Fork(insert e t1, v, t2)) - by definition of insert
= elem e (insert e t1) - by definition of elem
= true - by Inductive Hypothesis 

(* Case: e > v *)
	elem e (insert e Fork(t1, v, t2)) 
= elem e (Fork(t1, v , insert e t2)) - by definition of insert
= elem e (insert e t2) - by definition of elem
= true - by Inductive Hypothesis

(* Case: e = v *)
	elem e (insert e Fork(t1, v, t2)) 
= elem e (Fork(t1, v, t2)) - by definition of insert
= true - by definition of elem
```

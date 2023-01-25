(* Example: go.ml *)
open Opposite

module Opposites_List_Set = OppositeF (ListSet.ListSetM)
module Opposites_Tree_Set = OppositeF (TreeSet.TreeSetM)

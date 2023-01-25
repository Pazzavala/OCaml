# Opposites Attract
Examples:
- Modularity_via_Functors

### Significant Components
> - Utility Module
> - Set
> - List Set Module
> - Tree Set Module
> - Solution


### OCaml signatures Needed:
> - Set

### OCaml modules Needed:
> - Utility Module
> - List Set Module
> - Tree Set Module

### OCaml functors Nedded:
> - A functor to solve the opposites attract problem by choosin between a list set or a tree set implementation.


### Significant Functions and Data Types

##### Opposites
> Has type: string list -> (string * string) list
> - Will create many 6-character strings by replacing consonants with vowels in the words in input list of words.
> - For each these, it must check if that created string is in the word list or not.
> - to check if word is in list could use `List.mem`


##### Set
> - OCaml **signature** describing types and values that characterize a set
> - should be a name for this set type
> - a type for a function like `elem`
> - function for inserting elements into a set

##### List
> **Module** that implement this notion of sets using a list 

##### Binary Search Tree
> **Module** that implement this notion of sets using a binary tree
> Example:
> - whirldwind_parametric_tree.ml
> Functions for BST:
> - Function `elem` for checking if a value is in a tree.
> - Function to insert a value into a 'a tree


##### Utility Module
> - Sometimes you need some unrelated general purpose helper functions
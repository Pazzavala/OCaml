# Hwk 04: Addendum


### Utilities Module - DONE

Utility functions such as `read_file`, `implode`, `prefixes_suffixes`, and others are in a separate utilities file.

My Hwk_04 alredy had a utility.ml includes:

- prefixes_suffixes
- string_of_list
- string_of_pair
- print_answers
- implode
- explode
- read_words

- removed redundant `open ListSet` and  `open TreeSet`

Made `utility.ml` include a module `utilityM` for `warmup.ml`
- This means I had to change `Utility.read_words` to `Utility.UtilityM.read_words` in `opposites_list_set.ml` and `opposites_tree_set.ml`.


### Implementation of List-Based Sets - DONE

The implementation of the list-based sets should be in its own file.

- Already Done

### Implementation of Tree-Based Sets - DONE

The implementation of the tree-based sets should be in its own file.

- Already Done 

### Functor for Opposites - 
Created file `opposite.ml' here I defined functor `oppositeF (S: Set.SetS)`
Created functions:
- try_vowel
- try position
- try_word
- opposites

### Solution.ml Structure

```ocaml
module Opposites_List_Set = ...
module Opposites_Tree_Set = ...
```
- Applied the `oppositeF` functor with provided implementation of sets:
    - `ListSet.ListSetM`
    - `TreeSet.TreeSetM`
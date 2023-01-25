# Timing of Opposites
### opposites_list_set.native words-google-10000.txt
real    0m0.914s


### opposites_tree_set.native words-google-10000.txt
real    0m0.034s


### opposites_list_set.native words-google-10000-sorted.txt
real    0m0.812s


### opposites_tree_set.native words-google-10000-sorted.txt
real    0m0.418s


### Similarities and Differences Between Using TreeSet and ListSet:
Here we can see that when testing both the unsorted list and the sorted list of google words ListSet tends to run slower than TreeSet implementation.
But when testing the sorted list of words for both ListSet and TreeSet the opposites function for both improves on time.


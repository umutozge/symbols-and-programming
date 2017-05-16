# Trees

## Tree recursion

### Counting changes

Example from SICP:40. Compute the number of different ways a given amount of
money can be changed. Thinking a recursive procedure is considered easier than
an iterative one.

There are two ways to devise a recursive solution. 

1. Think over one type of denomination, say $$d$$. In any change of the amount
   $$a$$, there will be $$n$$ number of $$d$$s for $$0 \leq n \leq floor(a/n)$$, this
   exhausts the possiblities. So pick one of the denominations -- no matter
   which,

* [haskell](code/haskell/count-changes.hs)


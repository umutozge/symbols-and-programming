# Recursion 

## Tree recursion

### Counting changes

Example from SICP:40. Compute the number of different ways a given amount of
money can be changed. Thinking a recursive procedure is considered easier than
an iterative one.

There are two ways to devise a recursive solution. 

1. The reasoning in SICP is as follows:   
	* order the denominations; the order is not important, only have them
	  ordered -- let's say `[H,Q,D,N,P]` (we follow American names for
	  denominations). For any denomination `X` let `val(X)` denote the value of
	  the denominations, in terms of the lowest, namely penny (1 cent).
	* given an amount `a` and denominations `[d1...], the total number of (types of) changes is 
		* changes of `a` without using `d1`, namely changing `a` with [...]  plus
		* changes of `a-val(d1)` with `[d1...]` . 
	* this will result in a recursive process decrementing the amount and the
	  denomination types.
	* base cases of the recursion are:   
		* `0` amount, a successful change of the original amount;
		* `< 0` amount, an unsuccesful change attempt;
		* empty denomination list, again an unsuccessful change attempt.


2. The SICP solution is not the only way to devise a recursive code. Think over
   one type of denomination, say $$d$$. In any change of the amount $$a$$, there
   will be $$n$$ number of $$d$$s for $$ 0 \leq n \leq \lfloor a/n \rfloor$$,
   this exhausts the possiblities. So pick one of the denominations -- no matter
   which,

* [haskell](code/haskell/count-changes.hs)


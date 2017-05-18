# Recursion 

## Tree recursion

### Counting changes

Example from SICP:40. Compute the number of ways to change a given amount of
money using a set of denominations. Thinking a recursive procedure is considered easier than
an iterative one.

There are two ways to devise a recursive solution. 

1. The reasoning in SICP is as follows:   
	* order the denominations (henceforth denom(s)); the order is not important, only have them
	  ordered -- let's say `[H,Q,D,N,P]` (we use American names for
	  denoms). For any denom `X` let `val(X)` denote the value of
	  the denom.
	 	* given an amount `a` and denoms `[d_0,...,d_n]`, the total number of ways of changing is 
			* changes of `a` without using `d_0`, namely changing `a` with
		  	`[d_1,...,d_n]]`,  plus
			* changes of `a - val(d_0)` with `[d_0,...,d_n]`. 
	* this will result in a recursive process decrementing the amount and the
	  denomination types.
	* base cases of the recursion are:   
		* `0` amount, a successful change of the original amount;
		* `< 0` amount, an unsuccesful change attempt;
		* empty denomination list, again an unsuccessful change attempt.
	* trace the code by hand for  `20` and `[D,N,P]` 


2. The SICP solution is not the only way to devise a recursive code. Think over
   one type of denomination, say $$d$$. In any change of the amount $$a$$, there
   will be $$n$$ number of $$d$$s for $$ 0 \leq n \leq \lfloor a/n \rfloor$$,
   this exhausts the possiblities. So pick one of the denominations -- no matter
   which,

* [haskell](code/haskell/count-changes.hs)


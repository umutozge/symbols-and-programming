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

```haskell

module CC where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe


data Denomination = H | Q | D | N | P deriving (Ord,Eq,Show)

values = Map.fromList([(H,50),(Q,25),(D,10),(N,5),(P,1)])

type Amount = Integer


countChanges :: Amount -> [Denomination] -> Integer
countChanges amount denoms
    | amount == 0 = 1
    | amount < 0 = 0
   | null denoms = 0
    | otherwise = count4Denom amount denoms (head denoms) 

count4Denom :: Amount ->  [Denomination] -> Denomination -> Integer
count4Denom amount denoms denom = _count4Denom amount newDenoms denom
   where newDenoms = filter (\ x -> x /= denom) denoms

_count4Denom :: Amount -> [Denomination] -> Denomination -> Integer
_count4Denom amount denoms denom 
    | amount == 0 = 1
    | amount < 0 = 0
    | otherwise = countChanges amount denoms + _count4Denom  newAmount denoms denom 
    where    newAmount = amount - (Maybe.fromJust $ Map.lookup denom values)
              
removeFrom :: (Eq a) => a -> [a] -> [a]
removeFrom a as = filter (\ x -> x /= a) as    
```

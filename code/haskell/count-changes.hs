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

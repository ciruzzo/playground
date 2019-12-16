module Factor (factoring, factors)
where

import Data.List (group, unfoldr)

genfactor 1 = Nothing
genfactor n = let m = divisor n in Just (m, div n m)
  where divisor n = head $ filter (\x -> mod n x == 0) [2..]

-- [p..]
factoring = unfoldr genfactor

-- [[p,r]...]
factors = map (\x-> (head x, length x)) . group . factoring 

-- main = print $ factors 28

-- prime sieve
-- upto sqrt(m)  p*2,p*3,.. all done, O(n^0.5)
primes m = sieve [2..m]
  where sieve (p:xs) 
         | p*p > m   = p:xs
         | otherwise = p:sieve [x | x<-xs, mod x p /= 0]


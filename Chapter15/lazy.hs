fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) (fibs (tail fibs))

 --Newton
 infiniteList :: Double -> [(Double, Double)]
 infiniteList x = zip iterateList (tail iterateList)
    where iterateList = iterate (\n -> n + (x/2)) 1.0

 --use zip
 sqroot :: Double -> Double 
 sqroot x = head [i | (i,j) <- infiniteList x, (i - j) < 0.0001]
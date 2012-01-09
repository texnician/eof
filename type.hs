cir :: Float -> Float
cir r = 2 * pi * r

cir' :: Double -> Double
cir' r = 2 * pi * r

lucky :: (Integral a) => a -> String
lucky 7 = "YES"
lucky x = "NO"

factional :: (Integral a) => a -> a
factional 0 = 1
factional n = factional(n-1) * n

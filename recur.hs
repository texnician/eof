maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
  where maxTail = maximum' xs

replicate' :: Int -> a -> [a]
replicate' 0 x = []
replicate' n x = x:(replicate' (n-1) x)

replicate'' :: (Num i, Ord i) => i -> a -> [a]
replicate'' n x
  | n <= 0 = []
  | otherwise = x:replicate'' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x:take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ x:[]

reverse'' :: [a] -> [a]
reverse'' xs = iter [] xs
  where iter acc [] = acc
        iter acc (x:rest) = iter (x:acc) rest

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (p:xs) = qsort [x | x <- xs, x <= p ] ++ [p] ++ qsort [x | x <-xs, x > p]

insertSort :: (Ord a) => [a] -> [a]
insertSort [] = []
insertSort (x:xs) = insert x (insertSort xs)
  where insert :: (Ord a) => a -> [a] -> [a]
        insert x [] = [x]
        insert x (h:xs)
          | x < h = x:h:xs
          | otherwise = h:(insert x xs)

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort leftPart)  (mergeSort rightPart)
  where (leftPart, rightPart) = split xs

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] y = y
merge x [] = x
merge (x:xs) (y:ys)
  | x <= y = x:merge xs (y:ys)
  | otherwise = y:merge (x:xs) ys        

split :: [a] -> ([a], [a])
split [] = ([], [])
split [x] = ([x], [])
split xs@(x:y:ys) = advance xs ys
  where advance :: [a] -> [a] -> ([a], [a])
        advance (j:js) [] = ([j], js)
        advance (j:js) [z] = ([j], js)
        advance (j:js) (_:_:ks) = (j:sl, sr)
          where (sl, sr) = advance js ks

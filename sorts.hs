insSort :: Ord a => a -> [a] -> [a]
insSort x [] = [x]
insSort x (y : ys) | x <= y    = x : y : ys
                   | otherwise = y : insSort x ys

insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x : xs) = insSort x (insertionSort xs)



qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
	where
		smaller = [a | a <- xs, a <= x]
		larger  = [b | b <- xs, b > x]

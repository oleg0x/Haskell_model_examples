insSort :: Ord a => a -> [a] -> [a]
insSort x [] = [x]
insSort x (y : ys) | x <= y    = x : y : ys
                   | otherwise = y : insSort x ys

insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x : xs) = insSort x (insertionSort xs)



quickSort [] = []
quickSort (x:xs) = quickSort smaller ++ [x] ++ quickSort larger
	where
		smaller = [a | a <- xs, a <= x]
		larger  = [b | b <- xs, b > x]



merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys) =
	if x < y then x : merge xs (y : ys) else y : merge (x : xs) ys

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

mergeSort []  = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort ys) (mergeSort zs)
	where (ys, zs) = halve xs

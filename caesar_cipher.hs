import Data.Char

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']

rotateLeft :: Int -> [a] -> [a]
rotateLeft n xs = drop n xs ++ take n xs



shift :: Int -> Char -> Char
shift n c | isLower c = chr ((ord c - ord 'a' + n) `mod` 26 + ord 'a')
          | isUpper c = chr ((ord c - ord 'A' + n) `mod` 26 + ord 'a')
          | otherwise = c

encodeCaesar :: Int -> String -> String
encodeCaesar n xs = [shift n x | x <- xs]



table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4,
         6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

freqs :: String -> [Float]
freqs xs = [fromIntegral(count x xs) / fromIntegral(n) | x <- ['a'..'z']]
	where n = length [x | x <- xs, 'a' <= x && x <= 'z']

chiSquare :: [Float] -> [Float] -> Float
chiSquare os es = sum [((o - e)^2) / e | (o, e) <- zip os es]

decodeCaesar :: String -> String
decodeCaesar xs = encodeCaesar (-factor) xs where
	factor = head (positions (minimum chitab) chitab)
	chitab = [chiSquare (rotateLeft n table') table | n <- [0..25]]
	table' = freqs xs

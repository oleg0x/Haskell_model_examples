-- Recursion is the basic mechanism for looping in Haskell

fact :: Int -> Int
fact 0 = 1							-- Base case
fact n = n * fact (n - 1)			-- Recursive case

prod :: Num a => [a] -> a
prod [] = 1							-- Base case
prod (n : ns) = n * prod ns			-- Recursive case

len :: [a] -> Int
len [] = 0
len (_ : xs) = 1 + len xs			-- Wildcard pattern _ in the recursive case

rev :: [a] -> [a]
rev [] = []
rev (x : xs) = rev xs ++ [x]

(+++) :: [a] -> [a] -> [a]
[] +++ ys = ys
(x : xs) +++ ys = x : (xs +++ ys)

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []						-- 1st base case
zip' _ [] = []						-- 2nd base case
zip' (x : xs) (y : ys)  = (x, y) : zip' xs ys

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (_ : xs) = drop' (n - 1) xs

init' :: [a] -> [a]
init' [_] = []
init' (x : xs) = x : init' xs

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

evens :: [a] -> [a]
evens [] = []
evens (x : xs) = x : odds xs		-- Mutual recursion

odds :: [a] -> [a]
odds [] = []
odds (_ : xs) = evens xs			-- Mutual recursion

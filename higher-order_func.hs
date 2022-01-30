{-
A function that takes a function as an argument or returns a function
as a result is called a higher-order function.
In practice, the term 'higher-order' is often just used
for taking functions as arguments.
-}

import Prelude hiding (id, (.))



twice :: (a -> a) -> a -> a
twice f x = f (f x)



map1 :: (a -> b) -> [a] -> [b]
map1 f xs = [f x | x <- xs]

map2 :: (a -> b) -> [a] -> [b]
map2 f [] = []
map2 f (x:xs) = f x : map2 f xs



filter1 :: (a -> Bool) -> [a] -> [a]
filter1 pred xs = [x | x <- xs, pred x]

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 pred [] = []
filter2 pred (x : xs) | pred x    = x : filter2 pred xs
                      | otherwise =  filter2 pred xs



sumSqrEven :: [Int] -> Int
sumSqrEven ns = sum (map (^2) (filter even ns))



b1 = all even [0, 2, 4, 6, 8]
b2 = any odd [0, 2, 4, 6, 8]

li1 = takeWhile even [0, 2, 4, 6, 7, 8]
li2 = dropWhile odd [1, 3, 5, 6, 7, 8]



-- The higher-order library functions foldr and foldl encapsulates
-- the pattern of recursion for defining functions on lists.

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' func v [] = v
foldr' func v (x:xs) = func x (foldr' func v xs)
--foldr (#) v [x0, x1,..., xn] = x0 # (x1 # (... (xn # v)..))

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' func v [] = v
foldl' func v (x:xs) = foldl' func (func v x) xs
--foldl (#) v [x0,x1,...,xn] = (..((v # x0) # x1) ...) # xn



prod1 [] = 1
prod1 (x:xs) = x * prod1 xs

prod2 :: Num a => [a] -> a
-- prod2 xs = foldr (*) 1 xs		-- With explicit list xs
prod2 = foldr (*) 1					-- Operators must be parenthesised when used as arguments
-- prod2 = foldl (*) 1



and1 [] = True
and1 (x:xs) = x && and1 xs

and2 :: [Bool] -> Bool
-- and2 bs = foldr (&&) True bs		-- With explicit list bs
and2 = foldr (&&) True				-- Operators must be parenthesised when used as arguments
-- and2 = foldl (&&) True



length1 [] = 0
length1 (_:xs) = 1 + length xs

length2 :: [a] -> Int
length2 = foldr (\_ n -> 1 + n) 0
-- length2 = foldl (\n _ -> n + 1) 0



reverse' :: [a] -> [a]
reverse' = foldl (\xs x -> x:xs) []



-- Composition is used to reduce parentheses and avoid the initial argument.

(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)				-- Composition operator
-- (f . g) x = f (g x)



odd1 n = not (even n)
odd2 = not . even

twice1 f x = f (f x)
twice2 f = f . f

sumSqrEven1 ns = sum (map (^2) (filter even ns))
sumSqrEven2 = sum . map (^2) . filter even

someCalc = (+5) . (*2)



id :: a -> a
id = \x -> x
--id x = x

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

li1 = [x^2 | x <- [1..10]]				-- Comprehension notation
-- The symbol '|' is read as 'such that', '<-' is read as 'is drawn from',
-- and the expression x <- [1..10] is called a generator.

li2 = [(x,y) | x <- [1,2,3], y <- [4,5]]
li3 = [(x,y) | y <- [4,5], x <- [1,2,3]]
li4 = [(x,y) | x <- [1..4], y <- [x..4]]

concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs]

firsts :: [(a,b)] -> [a]
firsts ps = [x | (x,_) <- ps]

length' :: [a] -> Int
length' xs = sum [1 | _ <- xs]



li5 = [x | x <- [1..10], even x]		-- Guard to filter the values

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

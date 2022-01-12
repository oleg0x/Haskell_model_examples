import Data.List



votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

delDups :: Eq a => [a] -> [a]
delDups [] = []
delDups (x:xs) = x : filter (/= x) (delDups xs)

result :: Ord a => [a] -> [(Int, a)]
result vs = sort [(count v vs, v) | v <- delDups vs]

winner1 :: Ord a => [a] -> a
winner1 = snd . last . result



ballots :: [[String]]
ballots = [ ["Red", "Green"],
            ["Blue"],
            ["Green", "Red", "Blue"],
            ["Blue", "Green", "Red"],
            ["Green"] ]

delEmpty :: Eq a => [[a]] -> [[a]]
delEmpty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x xss = map (filter (/= x)) xss

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head
--rank xss = map snd (result (map head xss))

winner2 :: Ord a => [[a]] -> a
winner2 bs = case rank (delEmpty bs) of
	[c] -> c
	(c:cs) -> winner2 (elim c bs)

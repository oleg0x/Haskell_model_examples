inc :: [Int] -> [Int]
inc [] = []
inc (n:ns) = n+1 : inc ns

sqr :: [Int] -> [Int]
sqr [] = []
sqr (n:ns) = n^2 : sqr ns

-- Abstracting out this pattern gives the library function map:
inc' = map (+1)
sqr' = map (^2)



-- The class of types that support such a mapping function are called functors.
class Functor' f where		-- In the standard Prelude
	fmap' :: (a -> b) -> f a -> f b
-- Functor Laws
--	fmap id      = id					-- Preserve the identity function
--	fmap (g . h) = fmap g . fmap h		-- Preserve function composition
-- Functors abstract the idea of mapping a function over each element of a structure.


--  The type of lists can be made into a functor by defining fmap as map:
instance Functor' [] where
	-- fmap :: (a -> b) -> [a] -> [b]
	fmap' = map



-- data Maybe a = Nothing | Just a

instance Functor' Maybe where
	-- fmap :: (a -> b) -> Maybe a -> Maybe b
	fmap' _ Nothing = Nothing
	fmap' g (Just x) = Just (g x)



data Tree a = Leaf a | Node (Tree a) (Tree a)
	deriving Show

instance Functor' Tree where
	-- fmap :: (a -> b) -> Tree a -> Tree b
	fmap' g (Leaf x) = Leaf (g x)
	fmap' g (Node l r) = Node (fmap' g l) (fmap' g r)



instance Functor' IO where
	-- fmap :: (a -> b) -> IO a -> IO b
	fmap' g mx = do {x <- mx; return (g x)}



-- Generic function that can be used with any functor:
inc2 :: Functor' f => f Int -> f Int
inc2 = fmap' (+1)

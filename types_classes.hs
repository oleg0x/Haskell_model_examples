import Prelude hiding (Maybe, Nothing, Just)

-- Type declarations (synonyms for an existing types)

type String = [Char]

type Pos = (Int,Int)
type Trans = Pos -> Pos

type Pair a = (a,a)

type Assoc k v = [(k,v)]

find :: Prelude.Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k Prelude.== k']



-- Data declarations (completely new type)

data Bool' = False' | True'		-- Values of the type are called constructors
	deriving (Eq, Prelude.Ord, Show, Read)		-- Derived instance

data Move = North | South | East | West

move :: Move -> Pos -> Pos
move North (x, y) = (x, y+1)
move South (x, y) = (x, y-1)
move East  (x, y) = (x+1, y)
move West  (x, y) = (x-1, y)

data Shape = Circle Float | Rect Float Float

area :: Shape -> Float
area (Circle r) = pi * r^2		-- Circle is a constructor function
area (Rect x y) = x * y			-- Rect is a constructor function

data Maybe a = Nothing | Just a
	deriving (Show)

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)



-- Newtype declarations (New type without affecting performance)

newtype Nat = N Int				-- Single constructor with a single argument



-- Recursive types

data List' a = Nil | Cons a (List' a)

data Tree a    = Leaf a  | Node  (Tree a) a (Tree a)		-- Data in leaves and nodes
data Tree2 a   = Leaf2 a | Node2 (Tree2 a) (Tree2 a)		-- Data only in leaves
data Tree3 a   = Leaf3   | Node3 (Tree3 a) a (Tree3 a)		-- Data only in nodes
data Tree4 a b = Leaf4 a | Node4 (Tree4 a b) b (Tree4 a b)	-- Data of different types in leaves and nodes
data Tree5 a   = Node5 a [Tree5 a]							-- Have a list of subtrees

--      5
--   3    7
-- 1  4  6  9
t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5
	(Node (Leaf 6) 7 (Leaf 9))

occurs :: Prelude.Eq a => a -> Tree a -> Bool		-- For any trees
occurs x (Leaf y)      =  x Prelude.== y
occurs x (Node l y r)  =  x Prelude.== y || occurs x l || occurs x r

flatten :: Tree a -> [a]
flatten (Leaf x)      =  [x]
flatten (Node l x r)  =  flatten l ++ [x] ++ flatten r

occursBST :: Prelude.Ord a => a -> Tree a -> Bool	-- For binary search trees
occursBST x (Leaf y)                         =  x Prelude.== y
occursBST x (Node l y r)  |  x Prelude.== y  =  True
                          |  x Prelude.< y   =  occursBST x l
                          |  otherwise       =  occursBST x r



-- Classes and instances

class Equ a where
	(==), (/=) :: a -> a -> Bool
	x /= y = not (x Main.== y)
	x == y = not (x Main./= y)

instance Equ Bool where
	False == False  =  True			-- Default definition already has /= operator
	True  == True   =  True
	_     == _      =  False

class Equ a => Orde a where			-- Extension of the class Equ
	(<), (<=), (>), (>=) :: a -> a -> Bool
	min, max :: a -> a -> a
	min x y  |  x Main.<= y  =  x
	         |  otherwise    =  y
	max x y  |  x Main.<= y  =  y
	         |  otherwise    =  x

instance Orde Bool where
	False < True  =  True
	_     < _     =  False
	b <= c  =  (b Main.< c) || (b Main.== c)
	b > c   =  c Main.< b
	b >= c  =  c Main.<= b

double x = x + x			-- Function definition

quadruple x = double (double x)

factorial n = product [1..n]

twice f x = f (f x)

average1 ns = sum ns `div` length ns
average2 ns = div (sum ns) (length ns)



add (x, y) = x + y			-- Function on tuple, add :: Num a => (a,a) -> a
add' x y = x + y			-- Curried function, add' :: Num a => a -> a -> a

mul(x, y, z) = x * y * z	-- Function on tuple, mul :: (Int,Int,Int) -> Int
mul' x y z = x * y * z		-- Curried function, mul' :: Int -> (Int -> (Int -> Int))



even :: Integral a => a -> Bool
even n = n `mod` 2 == 0

recip :: Fractional a => a -> a
recip n = 1/n



sign :: Int -> Int
sign n = if n < 0 then -1 else		-- Conditional expressions
         if n == 0 then 0 else 1

sign' x | x < 0     = -1			-- Guarded equations
        | x == 0    = 0				-- The symbol '|' is read 'such that'
        | otherwise = 1



(&&) :: Bool -> Bool -> Bool
True && True   = True				-- Pattern matching
True && False  = False
False && True  = False
False && False = False

True &&& True = True
_ &&& _       = False				-- Wildcard pattern _

True &&&& b  = b
False &&&& _ = False

True # b  = b						-- The same but with the other symbol
False # _ = False

b ## c | b == c    = b
       | otherwise = False



second :: (a, b) -> b
second (_, y) = y					-- Tuple pattern

testA3 :: [Char] -> Bool
testA3 ['A', _, _] = True			-- List pattern ('A' and two symbols)
testA3 _           = False

x = [1, 2, 3]						-- Is just an abbreviation for
y = 1:(2:(3:[]))					-- operator ':' called 'cons' (construct)

testA :: [Char] -> Bool
testA ('A':_) = True				-- List pattern ('A' and any symbols)
testA _       = False

tail' :: [a] -> [a]
tail' (_:xs) = xs



xx = \x -> x + x					-- Lambda expression, '\' means lambda
-- > (\x -> x + x) 21  - Usage of the lambda expression

add1 :: Int -> Int -> Int
add1 x y = x + y

add2 :: Int -> (Int -> Int)
add2 = \x -> (\y -> x + y)			-- The same but using lambda expression

const' :: a -> (b -> a)
const' x = \_ -> x

odds :: Int -> [Int]
odds n = map f [0..n-1] where f x = 2*x + 1
odds' n = map (\x -> x*2 + 1) [0..n-1]

(+++) = \x -> (\y -> x + y)			-- Operator section

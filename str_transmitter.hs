import Data.Char

type Bit = Int			-- Bits in reverse order


bin2int1 :: [Bit] -> Int
bin2int1 bits = sum [w*b | (w, b) <- zip weights bits]
	where weights = iterate (*2) 1
-- iterate f x = [x, f x, f (f x), f (f (f x)), ...]

bin2int2 :: [Bit] -> Int
bin2int2 = foldr (\x y -> x + 2*y) 0


int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)


make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)
-- repeat :: a -> [a] produces an infinite list of copies of a value


encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)


chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)


decode :: [Bit] -> String
decode = map (chr . bin2int2) . chop8


channel :: [Bit] -> [Bit]
channel = id			-- Perfect communication channel :)


transmit :: String -> String
transmit = decode . channel . encode

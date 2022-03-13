-- An interactive program is viewed as a pure function that takes the current state
-- of the world as its argument, and produces a modified world as its result.

-- type IO = World -> World
-- type IO a = World -> (a, World)		-- More generally

-- Expressions of type IO a are called actions.

-- getChar :: IO Char
-- getChar = ...			-- The actual definition is built into GHC system

-- putChar :: Char -> IO ()
-- putChar c = ...			-- The actual definition is built into GHC system

-- The function "return" provides a bridge from pure expressions without
-- side effects to impure actions with side effects.

-- return :: a -> IO a
-- return v = ...

-- do v1 <- a1			-- Perform the action a1 and call its result value v1
--    v2 <- a2			-- Perform the action a2 and call its result value v2
--    v2 <- a2			-- Expressions vi <- ai are called generators
--    return (f v1 v2 ... vn)	-- Apply the function f to combine all results into a single value

-- Action reads three characters, discards the second, and returns the first and third as a pair.
act :: IO (Char, Char)
act = do x <- getChar
         getChar				-- Abbreviation for _ <- getChar
         y <- getChar			-- The layout rule is important
         return (x, y)			-- The layout rule is important

getLine' :: IO String
getLine' = do x <- getChar
              if x == '\n' then return []
              else
                  do xs <- getLine'
                     return (x : xs)

putStr' :: String -> IO ()
putStr' []       = return ()
putStr' (x : xs) = do putChar x
                      putStr' xs

putStrLn' :: String -> IO ()
putStrLn' xs = do putStr' xs
                  putChar '\n'

strlen :: IO ()
strlen = do putStr "Enter a string: "
            xs <- getLine'
            putStr "The string has "
            putStr (show (length xs))
            putStrLn " characters"

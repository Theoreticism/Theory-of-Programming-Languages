-- Haskell Caesar cipher program
-- Author: Christopher Lee

import Data.Char (ord, chr)

-- Rotates each character by turning them into ASCII values, applying
-- the algorithm, then turning them back into alphanumeric values.
rotate :: Char -> Int -> Char -> Char
rotate base offset char = chr $ ord base + (ord char - ord base + offset) `mod` 26

-- Encrypt function with pattern matching to determine upper or lower case.
encrypt :: Int -> String -> String
encrypt shift = map sentence
	where sentence char
		| char >= 'a' && char <= 'z' = rotate 'a' shift char
		| char >= 'A' && char <= 'Z' = rotate 'A' shift char
		| otherwise = char

-- Decrypt function taking the encrypt function with a negative shift.
decrypt :: Int -> String -> String
decrypt shift = encrypt (-shift)

-- Solve function with pattern matching recursion.
solve :: Int -> String -> IO ()
solve maxshift sentence
	| maxshift > 0 = do
		putStrLn ("Caesar " ++ show maxshift ++ ": " ++ encrypt maxshift sentence)
		solve (maxshift-1) sentence
	| otherwise = putStrLn ("Caesar " ++ show maxshift ++ ": " ++ sentence)

-- Main IO function
main :: IO ()
main = do
	putStrLn ("Original sentence: Hello world")
	putStrLn ("Shift amount: 3")
	putStrLn ("Encrypted sentence: " ++ encrypt 3 "Hello world")
	putStrLn ("Decrypted sentence: " ++ decrypt 3 (encrypt 3 "Hello world"))
	putStrLn ("Solving...")
	solve 26 "HAL"
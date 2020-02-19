module Basic where

import Data.List


-- The purpose of this lab is to get some experience writing Haskell
-- to solve some real world problems, since the best way to learn
-- a language is to try to write some code in it!  The goal of this
-- lab is not just to create code that works, but also to write code
-- that is stylish and concise.  Try to write small functions that
-- just perform a single task, and then combine those simple functions
-- into more complex functions.  Don't repeat yourself: write one
-- function for a task and reuse them as necessary.
--
-- The recommended way to run this code is to load it into ghci:
--
--      ghci Basic.hs
--
-- You will then be able to run most functions directly in the resulting
-- REPL.  If you make modifications to this file, type ':r' to reload
-- your changes in the repl.  Note that you will lose any local bindings
-- when you reload, so if you want some to persist put it in this file.
--
-- Credit: the structure of this assignment is inspired by
-- https://www.cis.upenn.edu/~cis194/spring13/hw/01-intro.pdf

-------------------------------------------------------------------

--                  Validating UPC codes

-------------------------------------------------------------------

-- Think about the last time you bought something from the grocery
-- store with a bar code on it.  The sequence of twelve digits you see
-- below it is what is called a UPC code, which is uniquely assigned to
-- each item you can buy at the store.  UPC codes have a "check digit",
-- the last digit in the UPC number, which can be used to verify that a
-- UPC code is well formed.
--
-- The way the check digit is computed is as follows:
--
--  1. Add all the digits in odd-numbered positions (first, third, etc.)
--     and multiply by three.  For example, given the code
--     036000241457, the odd numbered digits are 0+6+0+2+1+5=14;
--     multiplied by three, that gives 42.
--
--  2. Add all the digits in even-numbered positions (second, fourth,
--     etc., but NOT including the check digit) to the result.
--     Continuing the example, the even numbered digits are
--     3+0+0+4+4 = 11, giving us a total of 53.
--
--  3. Take the remainder of the result divided by ten.  If the
--     remainder is zero, use zero as the check digit; otherwise,
--     subtract the remainder from 10 to derive the check digit.
--     Finishing the example, we have 53 % 10 == 3, giving us
--     the check digit 7.
--
-- We are going to write a function that takes a UPC code as an integer
-- and checks if it is valid.

-- First, we need to find the digits of the UPC code.  Define a function
-- toDigits that converts a positive integer into a list of digits.
--
-- Example:
--  toDigits 1234 == [1,2,3,4]

toDigits :: Int -> [Int]
-- BEGIN toDigits (DO NOT DELETE THIS LINE)
toDigits n 
-- used division and module to extract array of digits
    | n < 1 = [0]
    | otherwise = toDigits(n `div` 10) ++ [n `mod` 10]
-- END toDigits (DO NOT DELETE THIS LINE)

-- Helper length function using pattern matching
lengthL :: (Num b) => [a] -> b
lengthL [] = 0
lengthL (_:xs) = 1 + lengthL xs

-- Helper sum function
sumElements :: [Int] -> Int
sumElements [] = 0
sumElements (x:xs) = x + sumElements xs


-- Since the integer 123 actually represents the UPC code 000000000123,
-- we need to pad the list of digits to this length.  Define a function
-- 'padZeros n' which pads a list of digits with zeros on the left until
-- its length is n, or does nothing if the list of digits is already
-- the greater to or equal than the desired length.
--
-- Example:
--  padZeros 4 [1,2] == [0,0,1,2]
--  padZeros 1 [1,2] == [1,2]

padZeros :: Int -> [Int] -> [Int]
-- BEGIN padZeros (DO NOT DELETE THIS LINE)
padZeros n list
-- padded zeros by appending 0 to front of list until length is of needed
    | n <= lengthL(list) = list
    | otherwise = 0 : padZeros(n-1) (list)
-- END padZeros (DO NOT DELETE THIS LINE)

-- Next, we need to extract the odd, even and check digits from the
-- digits.
--
-- Example:
--  oddDigits [1,2,3,4] = [1,3]
--  evenDigitsExcludingLast [1,2,3,4] = [2]
--  checkDigit [1,2,3,4] = 4

oddDigits :: [Int] -> [Int]
-- BEGIN oddDigits (DO NOT DELETE THIS LINE)
oddDigits [] = []
oddDigits [x] = [x]
-- extract odd indices by skipping over the even ones
oddDigits(x:y:xs) = x : (oddDigits xs)
-- END oddDigits (DO NOT DELETE THIS LINE)

-- helper function to extract even elements
helperEven :: [Int] -> [Int]
helperEven [] = []
helperEven [x] = []
-- extract even indices by skipping over the odd ones
helperEven(x:y:xs) = y : helperEven(xs)

evenDigitsExcludingLast :: [Int] -> [Int]
-- BEGIN evenDigitsExcludingLast (DO NOT DELETE THIS LINE)
evenDigitsExcludingLast list 
   | list == [] = []
   | lengthL list == 1 = []
   | otherwise = helperEven (init (list))
-- END evenDigitsExcludingLast (DO NOT DELETE THIS LINE)

checkDigit :: [Int] -> Int
-- BEGIN checkDigit (DO NOT DELETE THIS LINE)
checkDigit list
   | list == [] = 0
   -- get last numer of array
   | otherwise = last list 

-- END checkDigit (DO NOT DELETE THIS LINE)

-- Finally, put these functions together to define a function that
-- checks if an Int is a valid UPC code.  (Don't forget to check
-- that the code is representable in 12 digits!)
--
-- Example:
--  checkUPC 36000241457 == True
--  checkUPC 36000241458 == False

checkUPC :: Int -> Bool
-- BEGIN checkUPC (DO NOT DELETE THIS LINE)
checkUPC n =
    let list = toDigits n
        paddedList = padZeros 12 list
        odds = oddDigits paddedList
        evens = evenDigitsExcludingLast paddedList 
        r = ((sumElements(odds) * 3) + sumElements(evens)) `mod` 10
        digit = if r == 0 then 0 else (10-r)
    in digit == checkDigit list
-- END checkUPC (DO NOT DELETE THIS LINE)

-------------------------------------------------------------------

--                  Nonogram helper

-------------------------------------------------------------------

-- A nonogram is a kind of logic puzzle where you are given a 2D
-- grid annotated with numbers on each column and row specifying
-- the length of distinct strings of occupied cells:
--
--   Problem statement:          Solution:
--
--   |_|_|_|_|_|_|_|_| 3         |_|X|X|X|_|_|_|_| 3
--   |_|_|_|_|_|_|_|_| 2 1       |X|X|_|X|_|_|_|_| 2 1
--   |_|_|_|_|_|_|_|_| 3 2       |_|X|X|X|_|_|X|X| 3 2
--   |_|_|_|_|_|_|_|_| 2 2       |_|_|X|X|_|_|X|X| 2 2
--   |_|_|_|_|_|_|_|_| 6         |_|_|X|X|X|X|X|X| 6
--   |_|_|_|_|_|_|_|_| 1 5       |X|_|X|X|X|X|X|_| 1 5
--   |_|_|_|_|_|_|_|_| 6         |X|X|X|X|X|X|_|_| 6
--   |_|_|_|_|_|_|_|_| 1         |_|_|_|_|X|_|_|_| 1
--   |_|_|_|_|_|_|_|_| 2         |_|_|_|X|X|_|_|_| 2
--    1 3 1 7 5 3 4 3             1 3 1 7 5 3 4 3
--    2 1 5 1                     2 1 5 1
--
-- A classic strategy for solving nonograms is to look at the clue
-- for a single line and see if there are any squares which MUST
-- be black.  For example, consider this row of the nonogram
-- above:
--
--    |_|_|_|_|_|_|_|_| 6
--
-- The possible placements of the segment of six are:
--
--    |X|X|X|X|X|X|_|_| 6
--    |_|X|X|X|X|X|X|_| 6
--    |_|_|X|X|X|X|X|X| 6
--
-- From this, we can deduce the middle four cells must be filled in,
-- as they are filled in on every configuration.
--
-- We are going to write a simple nonogram helper which will perform
-- this deduction for a human solver.

-- To assist in testing your function, we've provided two convenience
-- functions which you can use to print your results to GHCi.

visualizeRow :: [Bool] -> IO ()
visualizeRow row = do
  let cell True = 'X'
      cell False = '_'
      contents = intersperse '|' (map cell row)
  putStrLn ("|" ++ contents ++ "|")

visualizeRows :: [[Bool]] -> IO ()
visualizeRows rows = mapM_ visualizeRow rows

-- First, define a function 'configs' which generates every possible
-- way of filling in a row (represented as a list of booleans) while
-- abiding to the number annotation.  You may find it helpful to
-- define some helper functions; you can place helper functions below
-- the main function you write.
--
--Example:
--
--  *Basic> visualizeRows (configs 10 [4,2])
--
--  |X|X|X|X|_|X|X|_|_|_|
--  |X|X|X|X|_|_|X|X|_|_|
--  |X|X|X|X|_|_|_|X|X|_|
--  |X|X|X|X|_|_|_|_|X|X|
--  |_|X|X|X|X|_|X|X|_|_|
--  |_|X|X|X|X|_|_|X|X|_|
--  |_|X|X|X|X|_|_|_|X|X|
--  |_|_|X|X|X|X|_|X|X|_|
--  |_|_|X|X|X|X|_|_|X|X|
--  |_|_|_|X|X|X|X|_|X|X|
--

configs :: Int -> [Int] -> [[Bool]]
-- BEGIN configs (DO NOT DELETE THIS LINE)
-- base cases when empty or when only one number in array (we only need to shift that number then)
configs n [] = [replicate n False]
configs n [x]
  | n < x = []
  | otherwise = [replicate x True ++ replicate (n-x) False] ++ map addFalse (configs (n-1) [x])
-- when list of numbers
configs n (x:xs)
  | (n > (sum xs + length xs)) && ((configs (n-x-1) xs) /= [replicate (n-x-1) False]) = map (replicate x True ++) (map addFalse ( configs (n-x-1) xs)) ++ map addFalse (configs (n-1) (x:xs))
  | otherwise = []


-- END configs (DO NOT DELETE THIS LINE)

addFalse  :: [Bool] -> [Bool]
addFalse list = False : list

-- Finally, write a function 'deduce' that returns a list of booleans,
-- with the entry True where that entry was True in every possible
-- configuration of the row.  (Hint: use a logical AND.)
--
-- Example:
--
--  *Basic> visualizeRow (deduce 10 [4,2])
--
--  |_|_|_|X|_|_|_|_|_|_|
--

deduce :: Int -> [Int] -> [Bool]
-- BEGIN deduce (DO NOT DELETE THIS LINE)
deduce n [] = []
deduce n x
  | n < 1 = []
  | otherwise = deducehelper matrix
  where matrix = (configs n x)

-- helper function we zip and AND the rows with the list of all trues
deducehelper :: [[Bool]] -> [Bool]
deducehelper [[]] = []
deducehelper (x:[]) = x
deducehelper (x:xs) = zipWith (&&) x (deducehelper xs)


-- END deduce (DO NOT DELETE THIS LINE)

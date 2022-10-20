-- {-# OPTIONS_GHC -Wall #-}
-- Murashko Artem SD20-01 --
-- Programming Paradigms Fall 2022 --
-- Lab session #8 -- 

import Text.Read (readMaybe)
import Data.Char (toUpper)
import System.Random (StdGen, newStdGen, randomRs)

main :: IO ()
main = print()

-- Ex 8.1

implies :: Bool -> Bool -> Bool
implies False _ = True
implies True y = y

-- True
test1_implies = implies (10 `mod` 6 == 0) (10 `mod` 0 == 0) 


-- Ex 8.2

imply :: [Bool] -> Bool -> Bool
imply [] _ = True
imply (x : xs) y = implies x (imply xs y)

-- imply (map (`divides` 100) [20, 4]) (3 `divides` 100) -- ⇒ False
-- imply (map (`divides` 100) [3, undefined]) 4 -- ⇒ True


-- Ex 8.3

cond :: [(Bool, a)] -> a -> a
cond [] val = val
cond ((True, a) : prs) val = a
cond ((False, _) : prs) val = cond prs val

-- "many apples"
test1_cond = let n = 12 in cond [ (n == 0, "no apples"), (n == 1, "one apple"), (n < 0, error "invalid input")] "many apples" 

-- hello
test2_cond = cond [] "hello"


-- Ex 8.4

chunksOf :: Int -> [a] -> [[a]]
chunksOf value [] = []
chunksOf value xs = [take value xs] ++ chunksOf value (drop value xs)

test1_chunksOf = chunksOf 3 [1..10] -- ⇒ [[1,2,3],[4,5,6],[7,8,9],[10]]
test2_chunksOf = chunksOf 3 "Hello, world!" -- ⇒ ["Hel","lo,"," wo","rld","!"]


-- Ex Random

randomDigits :: StdGen -> [Int]
randomDigits g = randomRs (0, 9) g

test_random = do
  -- generate a new seed
  g <- newStdGen
  -- print 10 random digits
  print (take 10 (randomDigits g))

  
-- Ex 8.6
  
every2nd :: [a] -> [a]
every2nd [] = []
every2nd (x1 : []) = []
every2nd (x1 : x2 : xs) = x2 : every2nd xs

test1_every2nd = every2nd [1..10] -- ⇒ [2,4,6,8,10]
test2_every2nd = every2nd "Hello, world!" -- ⇒ "el,wrd"
test3_every2nd = every2nd [undefined, 2] -- ⇒ [2]
test4_every2nd = take 3 (every2nd [1..]) -- ⇒ [2,4,6]

-- Ex 8.7

everyNth :: Int -> [a] -> [a]
everyNth _ [] = []
everyNth val xs = (drop (val - 1) (take val xs)) ++ everyNth val (drop val xs)

test1_everyNth = everyNth 2 [1..10] -- ⇒ [2,4,6,8,10]
test2_everyNth = everyNth 3 "Hello, world!" -- ⇒ "l,od"
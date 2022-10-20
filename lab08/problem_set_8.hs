-- {-# OPTIONS_GHC -Wall #-}
-- Murashko Artem SD20-01 --
-- Programming Paradigms Fall 2022 --
-- Problem Set #8 -- 

main :: IO ()
main = print()

-- Exercise 1

-- 1.a

isSingleton :: [a] -> Bool
isSingleton (x : []) = True
isSingleton _ = False

test1_isSingleton = isSingleton [1] -- True
test2_isSingleton = isSingleton [1..] -- False
test3_isSingleton = isSingleton [[1..]] -- True


-- 1.b

insert :: Int -> [Int] -> [Int]
insert val [] = [val]
insert val (x : xs) 
  | (val <= x) = [val, x] ++ xs
  | otherwise = x : (insert val xs)

test1_insert = insert 3 [1,2,5,7] -- [1,2,3,5,7]
test2_insert = insert 3 [0,1,1] -- [0,1,1,3]
test3_insert = take 5 (insert 3 [1..]) -- [1,2,3,3,4]


-- 1.c

separateBy :: a -> [a] -> [a]
separateBy _ (x : []) = [x]
separateBy sep (x : xs) = [x, sep] ++ separateBy sep xs

test1_separateBy = separateBy ',' "hello" -- "h,e,l,l,o"
test2_separateBy = take 5 (separateBy 0 [1..]) -- [1,0,2,0,3]


-- 1.d

splitWhenNot :: (a -> Bool) -> [a] -> ([a], [a])
splitWhenNot func xs = ((takeWhile func xs), (dropWhile func xs))

test1_splitWhenNot = splitWhenNot (/= ' ') "Hello, world!" -- ("Hello,"," world!")
test2_splitWhenNot = take 10 (fst (splitWhenNot (< 100) [1..])) -- [1,2,3,4,5,6,7,8,9,10]
test3_splitWhenNot = take 10 (snd (splitWhenNot (< 100) [1..])) -- [100,101,102,103,104,105,106,107,108,109]
test4_splitWhenNot = take 10 (fst (splitWhenNot (> 0) [1..])) -- [1,2,3,4,5,6,7,8,9,10]


-- 1.e

groupsSeparatedBy :: (a -> Bool) -> [a] -> [[a]]
groupsSeparatedBy _ [] = []
groupsSeparatedBy func xs = [fst splitted] ++ (groupsSeparatedBy func (removeFirst (snd splitted)))
  where 
    splitted = splitWhenNot (not.func) xs

    removeFirst :: [a] -> [a]
    removeFirst [] = []
    removeFirst (x:xs) = xs

test1_groupsSeparatedBy = groupsSeparatedBy (== ' ') "Here are some words!" -- ["Here","are","some","words!"]
test2_groupsSeparatedBy = groupsSeparatedBy (== ' ') "Hello!" -- ["Hello!"]


-- 1.f

replicateWithPos :: [a] -> [a]
replicateWithPos xs = helper 1 xs
  where 
    helper :: Int -> [a] -> [a]
    helper _ [] = []
    helper pos (x:xs) = (replicate pos x) ++ (helper (pos + 1) xs)

test1_replicateWithPos = replicateWithPos [1..3] -- [1,2,2,3,3,3]
test2_replicateWithPos = replicateWithPos "Hello" -- "Heelllllllooooo"
test3_replicateWithPos = take 10 (replicateWithPos [1..]) -- [1,2,2,3,3,3,4,4,4,4]


-- Exercise 2

-- 2.a

lucas :: [Int]
lucas = helper [2, 1]
  where
    helper :: [Int] -> [Int]
    helper (l : r : _) = l : (helper [r, (l + r)])

test1_lucase = take 10 lucas -- [2,1,3,4,7,11,18,29,47,76]


-- 2.b

approximationsOfRoot2 :: Double -> [Double]
approximationsOfRoot2 x = x : (approximationsOfRoot2 (x - x / 2 + 1 / x))

test1_approximationsOfRoot2 = take 5 (approximationsOfRoot2 1) -- [1.0,1.5,1.4166666666666665,1.4142156862745097,1.4142135623746899]

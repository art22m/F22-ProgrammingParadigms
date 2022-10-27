-- {-# OPTIONS_GHC -Wall #-}
-- Murashko Artem SD20-01 --
-- Programming Paradigms Fall 2022 --
-- Problem Set #9 -- 

main :: IO ()
main = print()

data Iter a
    = Done a -- ^ Final (computed) value.
    | Step (Iter a) -- ^ A computation that requires at least one more step.
    deriving (Show) -- for printing


-- Exercise 1

insert :: Int -> [Int] -> [Int]
insert val [] = [val]
insert val (x : xs) 
  | (val <= x) = [val, x] ++ xs
  | otherwise = x : (insert val xs)

test1_insert = insert 3 [1,2,5,7] -- [1,2,3,5,7]
test2_insert = insert 3 [0,1,1] -- [0,1,1,3]
test3_insert = take 5 (insert 3 [1..]) -- [1,2,3,3,4]


-- Exercise 2

approximate :: (a -> Bool) -> (a -> a) -> a -> Iter a
approximate checkFunc appFunc app 
    | checkFunc app = Done app
    | otherwise = Step (approximate checkFunc appFunc (appFunc app))

test1_approximate = approximate (\x -> x^2 < 1) (/ 2) 3 -- Step (Step (Done 0.75))
test2_approximate = approximate (\x -> x^2 < 0.01) (/ 2) 3 -- Step (Step (Step (Step (Step (Done 9.375e-2)))))
test3_approximate = approximate isSingleton (drop 1) [1..3] -- Step (Step (Done [3]))
    where
        isSingleton :: [a] -> Bool
        isSingleton (x : []) = True
        isSingleton _ = False


-- Exercise 3

-- 3.a

eval :: Iter a -> a
eval (Done val) = val
eval (Step iter) = eval iter

test1_eval = eval (approximate (\x -> x^2 < 0.01) (/ 2) 10) -- 7.8125e-2


-- 3.b

limit :: Int -> Iter a -> Iter (Maybe a)
limit 0 steps = Done Nothing
limit _ (Done val) = Done (Just val)
limit n (Step iter) = Step (limit (n - 1) iter)

test1_limit = limit 100 (approximate (\x -> x^2 < 0.01) (/ 2) 3) -- Step (Step (Step (Step (Step (Done (Just 9.375e-2))))))
test2_limit = limit 3 (approximate (\x -> x^2 < 0.01) (/ 2) 3) -- Step (Step (Step (Done Nothing)))
test3_limit = limit 0 (approximate (\x -> x^2 < 0.01) (/ 2) 3) -- Done Nothing


-- 3.c

partialEval :: Int -> Iter a -> Iter a
partialEval 0 iter = iter
partialEval _ (Done val) = Done val
partialEval n (Step iter) = partialEval (n - 1) iter

test1_partialEval = partialEval 100 (approximate (\x -> x^2 < 0.01) (/ 2) 3) -- Done 9.375e-2
test2_partialEval = partialEval 3 (approximate (\x -> x^2 < 0.01) (/ 2) 3) -- Step (Step (Done 9.375e-2))


--  3.d

steps :: Iter a -> Int
steps iter = helper iter 0
    where
        helper :: Iter a -> Int -> Int
        helper (Done _) n = n
        helper (Step iter) n = helper iter (n + 1)

test1_steps = steps (approximate (\x -> x^2 < 0.01) (/ 2) 3) -- 5
test2_steps = steps (approximate (\x -> x^2 < 1000) (/ 2) 3) -- 0


-- Exercise 4

-- 4.a

mapIter :: (a -> b) -> Iter a -> Iter b
mapIter func (Done a) = Done (func a)
mapIter func (Step iter) = Step(mapIter func iter)

test1_mapIter = mapIter (+1) (Done 3) -- Done 4
test2_mapIter = mapIter (+1) (Step (Step (Done 3))) -- Step (Step (Done 4))


-- 4.b

joinIter :: Iter (Iter a) -> Iter a
joinIter (Done (Done a)) = Done a
joinIter (Done (Step iter)) = Step iter
joinIter (Step iter) = Step (joinIter iter)

test1_joinIter = joinIter (Step (Done (Step (Done 3)))) -- Step (Step (Done 3))
test2_joinIter = joinIter (Done (Done 3)) -- Done 3


-- Exercise 5

insertIter :: Int -> [Int] -> Iter [Int]
insertIter val xs = helper val [] xs 
    where
        helper :: Int -> [Int] -> [Int] -> Iter [Int]
        helper val temp [] = Done (temp ++ [val])
        helper val temp (x : xs)
            |   (val <= x) = Step (Done (temp ++ [val, x] ++ xs))
            |   otherwise = Step (helper val (temp ++ [x]) xs)

test1_insertIter = insertIter 1 [2, 3] -- Step (Done [1,2,3])
test2_insertIter = insertIter 4 [2, 3] -- Step (Step (Done [2,3,4]))
test3_insertIter = insertIter 4 [1..10] -- Step (Step (Step (Step (Done [1,2,3,4,4,5,6,7,8,9,10]))))


-- Exercise 6

insertIter2 :: Int -> Iter [Int] -> Iter [Int]
insertIter2 val xs = helper val [] xs 
    where
        helper :: Int -> [Int] -> Iter [Int] -> Iter [Int]
        helper val temp (Done []) = Done (temp ++ [val])
        helper val temp (Done (x : xs))
            |   (val <= x) = Step (Done (temp ++ [val, x] ++ xs))
            |   otherwise = Step (helper val (temp ++ [x]) (Done xs))
        helper val temp (Step iter) = Step (helper val temp iter)

insertionSortIter :: [Int] -> Iter [Int]
insertionSortIter [] = Done []
insertionSortIter [x] = Done [x]
insertionSortIter (x : xs) = insertIter2 x (insertionSortIter xs)

test1_insertionSortIter = insertionSortIter [1..4] -- Step (Step (Step (Done [1,2,3,4])))
test2_insertionSortIter = insertionSortIter [4,3..1] -- Step (Step (Step (Step (Step (Step (Done [1,2,3,4]))))))
test3_insertionSortIter = steps (insertionSortIter [1..10]) -- 9
test4_insertionSortIter = steps (insertionSortIter [10,9..1]) -- 45

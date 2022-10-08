-- {-# OPTIONS_GHC -Wall #-}
-- Murashko Artem SD20-01 --
-- Programming Paradigms Fall 2022 --
-- Problem Set #6 -- 

main :: IO()

type Name = String
data Grade = A | B | C | D
data Student = Student Name Grade

data Result a
    = Success a
    | Failure String
    
dup f x = f x x
dip f x = f (f x x)
twice f x = f (f x)

-- Exercise 1

-- dup :: (a -> a -> b) -> a -> b
-- dip :: (a -> a -> a) -> a -> a -> a
-- twice :: (a -> a) -> a -> a


-- (1.a)

-- dip (+) 1 2
--
-- We have the following typing:
-- dip :: (a -> a -> a) -> a -> a -> a
-- (+) :: Int -> Int -> Int
-- 1 :: Int
-- 2 :: Int
--
-- Now we need to match the actual type of each argument of dip with the 
-- corresponding expected type:
-- (Int -> Int -> Int) = (a -> a -> a)
-- Thus, a :: Int
-- The type of the return value of dip is a, that is equal to Int
--
-- Answer: dip (+) 1 2 :: Int


-- (1.b)

-- dup (dip (+)) 1
-- 
-- dup :: (a1 -> a1 -> b1) -> a1 -> b1
-- dip :: (a2 -> a2 -> a2) -> a2 -> a2 -> a2
-- (+) :: Int -> Int -> Int
-- 1   :: Int
-- 
-- Let's apply (dip (+)):
-- (a2 -> a2 -> a2) = Int -> Int -> Int
-- Thus a2 :: Int, return type is (Int -> Int -> Int) = (a2 -> a2 -> a2)
-- 
-- Thus, evaluating dup (dip (+)) 1:
-- (a1 -> a1 -> b1) = (a2 -> a2 -> a2)
-- b1 = a2
-- b1 :: Int
--
-- Answer: dup (dip (+)) 1 :: Int


-- (1.c)

-- twice dip
-- 
-- twice :: (a1 -> a1) -> a1 -> a1
-- dip :: (a2 -> a2 -> a2) -> a2 -> a2 -> a2
--
-- Now we need to match the actual type of each argument of twice with the 
-- corresponding expected type:
-- (a1 -> a1) = (a2 -> a2 -> a2) -> a2 -> a2 -> a2
-- Thus, a1 = a2 -> a2 -> a2
--
-- The result of twice is a1 -> a1, which is equal
-- to (a2 -> a2 -> a2) -> (a2 -> a2 -> a2). 
--
-- Answer: twice dip :: (a2 -> a2 -> a2) -> (a2 -> a2 -> a2)


-- (1.d)

-- dip dip 
-- 
-- Let's name first dip as dip1, second dip as dip2, then:
-- dip1 dip2
-- dip1 :: (a1 -> a1 -> a1) -> a1 -> a1 -> a1
-- dip2 :: (a2 -> a2 -> a2) -> a2 -> a2 -> a2
--
-- Now we need to match the actual type of each argument of dip1 with the 
-- corresponding expected type:
-- (a1 -> a1 -> a1) = (a2 -> a2 -> a2) -> a2 -> a2 -> a2
-- a1 = (a2 -> a2 -> a2)
-- a1 = (a2 -> a2)
-- a1 = a2
--
-- Thus, a2 = (a2 -> a2 -> a2), or
-- a2 = a2 -> a2 -> a2
--    = (a2 -> a2 -> a2) -> (a2 -> a2 -> a2) -> (a2 -> a2 -> a2)
--    = (... -> ... -> ...) -> (... -> ... -> ...) -> (... -> ... -> ...) 
-- a2 is an infinite type.
--
-- Answer: dip dip produces type error


-- (1.e)

-- twice twice twice
--
-- Let's name first twice as twice1, second twice as twice2, third one as twice3, then:
-- twice1 twice2 twice3
-- twice1 :: (a1 -> a1) -> a1 -> a1
-- twice2 :: (a2 -> a2) -> a2 -> a2
-- twice3 :: (a3 -> a3) -> a3 -> a3
--
-- Now we need to match the actual type of each argument of twice1 with the 
-- corresponding expected type: 
-- (a1 -> a1) = (a2 -> a2) -> (a2 -> a2) => a1 = (a2 -> a2)
-- a1 = (a3 -> a3) -> a3 -> a3
--
-- Answer: twice1 twice2 twice3 :: (a3 -> a3) -> a3 -> a3


-- (1.f)

-- dup twice
--
-- dup :: (a1 -> a1 -> b1) -> a1 -> b1
-- twice :: (a2 -> a2) -> a2 -> a2
--
-- Now we need to match the actual type of each argument of dup with the 
-- corresponding expected type: 
-- (a1 -> a1 -> b1) = (a2 -> a2) -> a2 -> a2
-- a1 = (a2 -> a2)
-- a1 = a2
-- b1 = a2
-- Then,
-- b1 = a1
-- a1 = (a2 -> a2)
--    = (a1 -> a1)
--    = ((a1 -> a1) -> (a1 -> a1))
--    = ((... -> ...) -> (... -> ...))
-- a1 is an infinite type.
--
-- Answer: dup twice produces type error


-- Exercise 2

studentsWithA :: [Student] -> [Name]
studentsWithA [] = []
studentsWithA (Student name A : students) = name : studentsWithA students
studentsWithA (Student name grade : students) = studentsWithA students

-- main = print(studentsWithA [Student "Jack" B, Student "Jane" A]) 
-- ["Jane"]


-- Exercise 3

-- (3.a)

whileSuccess :: (a -> Result a) -> a -> a
whileSuccess f x = helper f (f x) x
  where
    helper :: (a -> Result a) -> (Result a) -> a -> a
    helper f (Success res) lstVal = helper f (f res) res
    helper f (Failure error_) lstVal = lstVal

f n | n > 100 = Failure "input is too large"
    | otherwise = Success (2 * n)
    
example1 = whileSuccess f 1 

-- main = print example1
-- 128


-- (3.b)

applyResult :: Result (a -> b) -> Result a -> Result b
applyResult (Success f) (Success arg) = Success (f arg)
applyResult (Failure error_) _ = Failure error_
applyResult _ (Failure error_) = Failure error_


-- (3.c)

fromResult :: (a -> b) -> (String -> b) -> Result a -> b
fromResult f1 f2 (Success res) = f1 res
fromResult f1 f2 (Failure error_) = f2 error_


-- (3.c)

combineResultsWith :: (a -> b -> c) -> Result a -> Result b -> Result c
combineResultsWith f (Success res1) (Success res2) = Success(f res1 res2)
combineResultsWith f _ (Failure error_) = Failure error_
combineResultsWith f (Failure error_) _ = Failure error_

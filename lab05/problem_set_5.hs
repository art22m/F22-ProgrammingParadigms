-- {-# OPTIONS_GHC -Wall #-}
-- Murashko Artem SD20-01 --
-- Programming Paradigms Fall 2022 --
-- Problem Set #5 -- 

type Bit = Int

main :: IO()

-- Exercise 1

-- 1.a

binaryToDecimalReversed :: [Bit] -> Int
binaryToDecimalReversed [] = 0
binaryToDecimalReversed (bit:bits) = bit + 2 * (binaryToDecimalReversed bits)

binaryToDecimal :: [Bit] -> Int
binaryToDecimal [] = 0
binaryToDecimal bits = binaryToDecimalReversed (reverse bits)

main = print(binaryToDecimal [1, 0, 1, 1, 0]) -- 22


-- 1.b

skipLeadingZeros :: [Bit] -> [Bit]
skipLeadingZeros [] = []
skipLeadingZeros (0:bits) = skipLeadingZeros bits
skipLeadingZeros (1: bits) = 1 : bits

countAllZeros :: [Bit] -> Int
countAllZeros [] = 0
countAllZeros (0:bits) = 1 + countAllZeros bits
countAllZeros (1:bits) = countAllZeros bits

countZeros :: [Bit] -> Int
countZeros bits = countAllZeros(skipLeadingZeros bits)

-- main = print(countZeros [0, 0, 0, 1, 0, 1, 1, 0]) -- 2


-- 1.c

encodeWithLengths :: [Bit] -> [Bit]
encodeWithLengths [] = []
encodeWithLengths (0:bits) = encodeWithLengths(bits)
encodeWithLengths (1:bits) = helper 1 1 bits
  where 
    helper :: Bit -> Int -> [Bit] -> [Bit]
    helper bit cnt [] = [cnt]
    helper 1 cnt (1:bits) = helper 1 (cnt + 1) bits
    helper 0 cnt (0:bits) = helper 0 (cnt + 1) bits
    helper bit cnt (b:bits) =  cnt : (helper b 1 bits)
    
-- main = print(encodeWithLengths [0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0]) -- [2,1,3,2]


-- 1.d

binaryOdd :: [Bit] -> Bool 
binaryOdd [] = False
binaryOdd (1:[]) = True
binaryOdd (0:[]) = False
binaryOdd (bit:bits) = binaryOdd bits

-- main = print(binaryOdd [1, 0, 1, 1, 0]) -- False
-- main = print(binaryOdd [1, 0, 1, 1, 1]) -- True


-- 1.e

invertBits :: [Bit] -> [Bit]
invertBits [] = []
invertBits (0:bits) = 1:invertBits(bits)
invertBits (1:bits) = 0:invertBits(bits)

append :: Bit -> [Bit] -> [Bit]
append a [] = [a]
append a (bit:bits) = bit : append a bits

decrement :: [Bit] -> [Bit]
decrement bits = makeZeroIfNeeded(skipLeadingZeros(reverse (helper [] (reverse bits))))
  where 
    helper :: [Bit] -> [Bit] -> [Bit]
    helper left [] = [0]
    helper left (1:bits) = (append 0 (invertBits left)) ++ bits
    helper left (0:bits) = helper (0:left) bits
    
    makeZeroIfNeeded :: [Bit] -> [Bit]
    makeZeroIfNeeded [] = [0]
    makeZeroIfNeeded bits = bits

-- main = print(decrement [1, 0, 1, 1, 0]) -- [1,0,1,0,1]
-- main = print(decrement [1, 0, 0, 0, 0]) -- [1,1,1,1]
-- main = print(decrement [0]) -- [0]


-- 1.f

propagate :: (Bool, [Int]) -> [(Bool, Int)] 
propagate (flag, []) = []
propagate (flag, (x:xs)) = (flag, x) : propagate (flag, xs)

-- main = print(propagate (False, [1, 2, 3]) ) -- [(False,1),(False,2),(False,3)]
-- main = print(propagate (True, [1, 1]) ) -- [(True,1),(True,1)]


-- Exercise 2

-- 2.a

alternatingSum :: [Int] -> Int
alternatingSum [] = 0
alternatingSum (xl:xr:xs) = xl - xr  + (alternatingSum xs)
alternatingSum (x:[]) = x

-- main = print(alternatingSum [6, 2, 4, 1, 3, 9]) -- 1


-- 2.b

-- alternatingSum [1,2,3,4,5]
-- 1 - 2 + (alternatingSum [3,4,5])
-- 1 - 2 + (3 - 4 + (alternatingSum [5]))
-- 1 - 2 + (3 - 4 + 5)
-- 1 - 2 + 4
-- 3


-- Exercise 3

data Radians = Radians Double deriving Show
data Degrees = Degrees Double deriving Show

_pi :: Double
_pi = 3.14159
    
toDegrees :: Radians -> Degrees
toDegrees (Radians radians) = Degrees(radians * (180.0 / _pi))

fromDegrees :: Degrees -> Radians
fromDegrees (Degrees degrees) = Radians(degrees * (_pi / 180.0))

-- main = print(toDegrees (Radians 1)) -- 57.295827908797776
-- main = print(fromDegrees (Degrees 180)) -- 3.14159


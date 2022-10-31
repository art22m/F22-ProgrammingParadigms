-- {-# OPTIONS_GHC -Wall #-}
-- Murashko Artem SD20-01 --
-- Programming Paradigms Fall 2022 --
-- Homework Assignment 02 -- 

main :: IO ()
main = print()

-- 1.1 Lines

-- A line with a focus.
-- Line xs y zs represents a descrete line:
-- * xs represents all elements to the left (below)
-- * y is the element in focus
-- * zs represents all elements after (above)
data Line a = Line [a] a [a] deriving Show -- required to enable printing (for finite lines)

-- | A line of integers with focus at 0.
integers :: Line Integer
integers = Line [-1, -2..] 0 [1, 2..]


-- Exercise 1.1

-- Keep up to a given number of elements in each direction in a line.
cutLine :: Int -> Line a -> Line a
cutLine n (Line xs y zs) = Line (take n xs) y (take n zs)

test1_cutLine = cutLine 3 integers -- Line [-1,-2,-3] 0 [1,2,3]
test2_cutLine = cutLine 2 (Line [-1] 0 [1])


-- Exercise 1.2 

-- Generate a line by using generating functions. (genLine f x g) generates a line with x in its focus,
-- then it applies f to x until reaching Nothing to produce a list of elements to the left of x,
-- and, similarly, applies g to x until reaching Nothing to produce a list of elements to the right of x.
genLine :: (a -> Maybe a) -> a -> (a -> Maybe a) -> Line a
genLine leftFunc x rightFunc = Line (genFunc leftFunc (leftFunc x)) x (genFunc rightFunc (rightFunc x))
    where
        genFunc :: (a -> Maybe a) -> Maybe a -> [a]
        genFunc func (Just element) = element : (genFunc func (func element))
        genFunc _ Nothing = []

test1_genLine = genLine (applyIf (> -3) (subtract 1)) 0 (applyIf (< 3) (+1)) -- Line [-1,-2,-3] 0 [1,2,3]
    where
        applyIf :: (a -> Bool) -> (a -> b) -> a -> Maybe b
        applyIf p f x
            | p x       = Just (f x)
            | otherwise = Nothing

test2_genLine = genLine (\_ -> Nothing) 1 (\_ -> Nothing) -- Line [] 1 []
test3_genLine = cutLine 3 (genLine Just 0 Just) -- Line [0,0,0] 0 [0,0,0]


-- Exercise 1.3

-- Apply a function to all elements on a line.
mapLine :: (a -> b) -> Line a -> Line b
mapLine func (Line xs y zs) = Line (map func xs) (func y) (map func zs)

test1_mapLine = mapLine (^2) integers -- Line [1, 4, 9, ..] 0 [1, 4, 9, ..]


-- Exercise 1.4

-- Zip together two lines.
zipLines :: Line a -> Line b -> Line (a, b)
zipLines (Line xs1 y1 zs1) (Line xs2 y2 zs2) = Line (zip xs1 xs2) (y1, y2) (zip zs1 zs2)

test1_zipLines = cutLine 5 (zipLines integers integers) -- Line [(-1,-1),(-2,-2),(-3,-3)] (0,0) [(1,1),(2,2),(3,3)]

-- Zip together two lines with a given combining function.
zipLinesWith :: (a -> b -> c) -> Line a -> Line b -> Line c
zipLinesWith func (Line xs1 y1 zs1) (Line xs2 y2 zs2) = Line (zipWith func xs1 xs2) (func y1 y2) (zipWith func zs1 zs2)

test1_zipLinesWith = cutLine 5 (zipLinesWith (*) integers integers) -- Line [1,4,9,16,25] 0 [1,4,9,16,25]


-- 1.2 Rule 30

-- Alive = 1 | Dead = 0
-- current pattern	           111 110 101 100 011 010 001 000
-- new state for center cell	0	0	0	1	1	1	1	0

data Cell = Alive | Dead deriving Show

-- Exercise 1.5
    
-- Function computes next state according to Rule 30 with given current state and the state of its two neighbors
-- Current state is in the middle - (Neighbor, Curr_state_ Neighbor)
getRule30State :: Cell -> Cell -> Cell -> Cell
getRule30State Alive Alive _ = Dead
getRule30State Alive Dead Alive = Dead
getRule30State Alive Dead Dead = Alive
getRule30State Dead Alive _ = Alive
getRule30State Dead Dead state = state

-- Function computes next state of a focus for given line.
rule30 :: Line Cell -> Cell
rule30 (Line [] y []) = getRule30State Dead y Dead
rule30 (Line (x:_) y []) = getRule30State x y Dead
rule30 (Line [] y (z:_)) = getRule30State Dead y z
rule30 (Line (x:_) y (z:_)) = getRule30State x y z

test1_rule30 = rule30 (Line [Dead, Alive, Alive] Alive [Alive, Alive, Alive]) -- Alive
test2_rule30 = rule30 (Line [Alive, Alive, Alive] Alive [Alive, Alive, Alive]) -- Dead


-- Exercise 1.6
-- Shifts the focus on the line by one position (if possible)

shiftLeft :: Line a -> Maybe (Line a)
shiftLeft (Line (x:xs) y zs) = Just (Line xs x (y : zs))
shiftLeft (Line [] _  _) = Nothing

test1_shiftLeft = shiftLeft (Line [0,1,1] 1 [1,1,1]) -- Just (Line [1,1] 0 [1,1,1,1])
test2_shiftLeft = shiftLeft (Line [0] 1 []) -- Just (Line [] 0 [1])

shiftRight :: Line a -> Maybe (Line a)
shiftRight (Line xs y (z:zs)) = Just (Line (y : xs) z zs)
shiftRight (Line _ _ []) = Nothing

test1_shiftRight = shiftRight (Line [0,1,1] 1 [1,1,1]) -- Just (Line [1,0,1,1] 1 [1,1])
test2_shiftRight = shiftRight (Line [0,1,1] 1 []) -- Nothing


-- Exercise 1.7

-- lineShifts function maps every cell in a line into a version of the original line where that cell is in focus. 
-- The new line of lines should have the original line in focus.
lineShifts :: Line a -> Line (Line a)
lineShifts line = Line (helperLeft (shiftLeft line)) line (helperRight (shiftRight line))
    where
        helperLeft :: Maybe(Line a) -> [Line a]
        helperLeft Nothing = []
        helperLeft (Just line) = line : (helperLeft (shiftLeft line))

        helperRight :: Maybe(Line a) -> [Line a]
        helperRight Nothing = []
        helperRight (Just line) = line : (helperRight (shiftRight line))
        

test1_lineShifts = lineShifts (Line [2,1] 3 [4,5])
-- Line [Line [1] 2 [3,4,5],Line [] 1 [2,3,4,5]] (Line [2,1] 3 [4,5]) [Line [3,2,1] 4 [5],Line [4,3,2,1] 5 []]
    
-- With lineShifts, we can now simply apply rule30 to each shifted version of the line to get the new state for each cell:
applyRule30 :: Line Cell -> Line Cell
applyRule30 line = mapLine rule30 (lineShifts line)

test1_applyRule30 = cutLine 4 $ applyRule30 (Line ([Dead, Alive, Alive] ++ repeat Dead) Alive ([Alive, Alive, Alive] ++ repeat Dead)) 
-- Line [Dead,Dead,Alive,Alive] Alive [Dead,Dead,Dead,Alive]
{-# OPTIONS_GHC -Wall #-}
-- Murashko Artem SD20-01 --
-- Programming Paradigms Fall 2022 --
-- Homework Assignment 02 -- 

-- import CodeWorld

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
        

-- Line [Line [1] 2 [3,4,5],Line [] 1 [2,3,4,5]] (Line [2,1] 3 [4,5]) [Line [3,2,1] 4 [5],Line [4,3,2,1] 5 []]
test1_lineShifts = lineShifts (Line [2,1] 3 [4,5])
    
-- With lineShifts, we can now simply apply rule30 to each shifted version of the line to get the new state for each cell:
applyRule30 :: Line Cell -> Line Cell
applyRule30 line = mapLine rule30 (lineShifts line)

-- Line [Dead,Dead,Alive,Alive] Alive [Dead,Dead,Dead,Alive]
test1_applyRule30 = cutLine 4 $ applyRule30 (Line ([Dead, Alive, Alive] ++ repeat Dead) Alive ([Alive, Alive, Alive] ++ repeat Dead)) 

-- -- Exercise 1.8

-- -- Helpers 

-- -- rectangle side size
-- recSize :: Double
-- recSize = 2 

-- -- black rectagle, i.e. alive cell
-- blackSquare :: Picture
-- blackSquare = (colored black (solidRectangle (recSize - 0.1) (recSize - 0.1)))

-- -- white rectagle, i.e. dead cell
-- whiteSquare :: Picture
-- whiteSquare = (rectangle (recSize - 0.12) (recSize - 0.12))

-- -- transforms line of cells to line of pictures
-- toLineOfPictures :: Line Cell -> Line Picture
-- toLineOfPictures line = mapLine (\x -> toPicture x) line
--   where
--     toPicture :: Cell -> Picture
--     toPicture Alive = blackSquare
--     toPicture Dead = whiteSquare

-- -- render list of squares from left to right
-- renderPicturesListRight :: [Picture] -> Picture 
-- renderPicturesListRight [] = blank
-- renderPicturesListRight (pic:pics) = pic <> (translated recSize 0 (renderPicturesListRight pics))

-- -- render list of squares from right to left 
-- renderPicturesListLeft :: [Picture] -> Picture 
-- renderPicturesListLeft [] = blank
-- renderPicturesListLeft (pic:pics) = pic <> (translated (negate recSize) 0 (renderPicturesListLeft pics))

-- -- Render a line of 1x1 pictures.
-- renderLine :: Line Picture -> Picture
-- renderLine (Line xs y zs) = leftPicture <> y <> rightPicture
--   where 
--       leftPicture :: Picture
--       leftPicture = (translated (negate recSize) 0 (renderPicturesListLeft xs))
      
--       rightPicture :: Picture
--       rightPicture = (translated recSize 0 (renderPicturesListRight zs))
      
-- -- Render the fist N steps of Rule 30, applied to a given starting line.
-- renderRule30 :: Int -> Line Cell -> Picture
-- renderRule30 n line
--   | n > 1 = renderLine (toLineOfPictures line) <> (translated 0 (negate recSize) (renderRule30 (n - 1) (applyRule30 line)))
--   | otherwise = renderLine (toLineOfPictures line)

-- -- Test examples:
-- -- main = drawingOf (renderRule30 8 (Line [Dead, Dead, Dead, Dead, Dead, Dead, Dead] Alive [Dead, Dead, Dead, Dead, Dead, Dead, Dead]))


-- 1.3 Discrete spaces
-- A discrete 2D space can be represented by a (vertical) line of (horizontal) lines:
-- A 2D space is merely a (vertical) line, where each element is a (horizontal) line.

data Space a = Space (Line (Line a)) deriving Show

-- Exercise 1.10 

integers_cutted = cutLine 4 integers
integers_space = Space (Line [integers_cutted, integers_cutted] integers_cutted [integers_cutted, integers_cutted])

-- Apply a function to all elements on a space.
mapSpace :: (a -> b) -> Space a -> Space b
mapSpace func (Space line) = Space(mapLine (mapLine func) line)

-- Space (Line [Line [1,4,9,16] 0 [1,4,9,16],Line [1,4,9,16] 0 [1,4,9,16]] (Line [1,4,9,16] 0 [1,4,9,16]) [Line [1,4,9,16] 0 [1,4,9,16],Line [1,4,9,16] 0 [1,4,9,16]])
test1_mapSpace = mapSpace (^2) integers_space

-- Zip together two spaces.
zipSpaces :: Space a -> Space b -> Space (a, b)
zipSpaces (Space line1) (Space line2) = Space(mapLine helper (zipLines line1 line2))
    where 
        helper :: (Line a, Line b) -> Line (a, b)
        helper (left, right) = zipLines left right

-- Too long output =)
test1_zipSpaces = zipSpaces integers_space (mapSpace negate integers_space)

-- Zip together two spaces with a given combining function.
zipSpacesWith :: (a -> b -> c) -> Space a -> Space b -> Space c
zipSpacesWith func space1 space2 = mapSpace (helper func) (zipSpaces space1 space2)
    where
        helper :: (a -> b -> c) -> (a, b) -> c 
        helper tempFunc (left, right) = tempFunc left right

-- Space (Line [Line [0,0,0,0] 0 [0,0,0,0],Line [0,0,0,0] 0 [0,0,0,0]] (Line [0,0,0,0] 0 [0,0,0,0]) [Line [0,0,0,0] 0 [0,0,0,0],Line [0,0,0,0] 0 [0,0,0,0]])
test1_zipSpacesWith = zipSpacesWith (+) integers_space (mapSpace negate integers_space)

-- Too long output =)
test2_zipSpacesWith = zipSpacesWith (\x y -> 2*x + y) integers_space integers_space


-- Bonus: Function computing the next state of the cell in focus for space, according to the rules30:
--
-- getFocuses :: [Line Cell] -> [Cell]
-- getFocuses lines = foldr helper [] lines
--     where
--         helper :: Line Cell -> [Cell] -> [Cell]
--         helper (Line xs y zs) lst = y : lst

-- rule30Space :: Space Cell -> Cell
-- rule30Space (Space(Line left (Line _ y _) right)) = rule30 (Line (getFocuses left) y (getFocuses right))

-- 1.4 Conway’s Game of Life

cellsLine1 = (Line [Dead, Alive, Alive] Dead [Alive, Alive, Alive])
cellsLine2 = (Line [Dead, Alive, Alive] Alive [Alive, Alive, Alive])

-- Exercise 1.12 
-- Function computes next state of a focuses for given space, according to the rules of Conway’s Game of Life:

cellToInt :: Cell -> Int 
cellToInt Alive = 1
cellToInt Dead = 0

getConwayRuleState :: Cell -> Int -> Cell
getConwayRuleState Dead 3 = Alive
getConwayRuleState Alive 2 = Alive
getConwayRuleState Alive 3 = Alive
getConwayRuleState Alive _ = Dead
getConwayRuleState Dead _ = Dead

conwayRule :: Space Cell -> Cell
conwayRule (Space(Line left (Line xs y ys) right)) = getConwayRuleState y (countAliveNeighbours left (Line xs y ys) right)
  where
    countAliveNeighbours :: [Line Cell] -> Line Cell -> [Line Cell] -> Int
    countAliveNeighbours [] f [] = countAliveNeighborsWithoutFocus f 
    countAliveNeighbours [] f (r:_) = (countAliveNeighborsWithoutFocus f) + (countAliveNeighborsWithFocus r)
    countAliveNeighbours (l:_) f []  = (countAliveNeighborsWithFocus l) + (countAliveNeighborsWithoutFocus f)
    countAliveNeighbours (l:_) f (r:_) = (countAliveNeighborsWithFocus l) + (countAliveNeighborsWithoutFocus f) + (countAliveNeighborsWithFocus r)

    -- Take left and right elements from focus, and calculate alive number
    countAliveNeighborsWithoutFocus :: Line Cell -> Int
    countAliveNeighborsWithoutFocus (Line [] _ []) = 0
    countAliveNeighborsWithoutFocus (Line (l:_) _ []) = cellToInt l
    countAliveNeighborsWithoutFocus (Line [] _ (r:_)) = cellToInt r
    countAliveNeighborsWithoutFocus (Line (l:_) _ (r:_)) = (cellToInt l) + (cellToInt r)

    -- Take left, right elements from focus and focus, then calculate alive number
    countAliveNeighborsWithFocus :: Line Cell -> Int
    countAliveNeighborsWithFocus (Line [] f []) = cellToInt f
    countAliveNeighborsWithFocus (Line (l:_) f []) = (cellToInt l) + (cellToInt f)
    countAliveNeighborsWithFocus (Line [] f (r:_)) = (cellToInt f) + (cellToInt r)
    countAliveNeighborsWithFocus (Line (l:_) f (r:_)) = (cellToInt l) + (cellToInt f) + (cellToInt r)


-- Exercise 1.13

-- Shifts the focus on the space to the left by one position (if possible)
shiftSpaceLeft :: Space a -> Maybe (Space a)
shiftSpaceLeft (Space (Line left focus right)) = helper (shiftLeft focus) (Line left focus right)
    where 
        helper :: Maybe (Line a) -> Line (Line a) -> Maybe (Space a)
        helper Nothing _ = Nothing
        helper (Just _) line = Just (Space(mapLine shift (line)))

        shift :: Line a -> Line a
        shift (Line (x:xs) y zs) = (Line xs x (y : zs))
        shift line = line -- impossible with check (not good)

-- Shifts the focus on the space to the right by one position (if possible)
shiftSpaceRight :: Space a -> Maybe (Space a)
shiftSpaceRight (Space (Line left focus right)) = helper (shiftLeft focus) (Line left focus right)
    where 
        helper :: Maybe (Line a) -> Line (Line a) -> Maybe (Space a)
        helper Nothing _ = Nothing
        helper (Just _) line = Just (Space(mapLine shift (line)))

        shift :: Line a -> Line a
        shift (Line xs y (z:zs)) = (Line (y : xs) z zs)
        shift line = line -- impossible with check (not good)

-- Function that converts each cell in a discrete space into a version of the original space with focus shifted to that cell. 
-- The new space (of spaces) must have the original space in focus.

-- spaceShifts :: Space a -> Space (Space a)
-- spaceShifts space = Space (helperLeft (shiftSpaceLeft space)) space (helperRight (shiftSpaceRight space))
--     where
--         helperLeft :: Maybe(Space a) -> [Space a]
--         helperLeft Nothing = []
--         helperLeft (Just space) = space : (helperLeft (shiftSpaceLeft space))

--         helperRight :: Maybe(Space a) -> [Space a]
--         helperRight Nothing = []
--         helperRight (Just space) = space : (helperRight (shiftSpaceRight space))

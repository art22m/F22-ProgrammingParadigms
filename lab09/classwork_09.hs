-- {-# OPTIONS_GHC -Wall #-}
-- Murashko Artem SD20-01 --
-- Programming Paradigms Fall 2022 --
-- Problem Set #8 -- 

main :: IO ()
main = print()

-- Exercise 6.1

exercise_6_1_a :: Integer
exercise_6_1_a = 2 + 3

exercise_6_1_b :: (Double, Double) -> Double
exercise_6_1_b (x, y) = sqrt (x^2 + y^2)

exercise_6_1_c :: a -> a -> [a]
exercise_6_1_c x y = [x, y]

exercise_6_1_d :: [Char] -> [Char]
exercise_6_1_d [] = ""
exercise_6_1_d (x:xs) = exercise_6_1_d xs ++ [x]

exercise_6_1_e :: a -> (a, a)
exercise_6_1_e x = (x, x)


-- Exercise 6.2

data Cartesian = Cartesian Double Double deriving Show
data Radians = Radians Double deriving Show
data Polar = Polar Double Radians deriving Show

toPolar :: Cartesian -> Polar
toPolar (Cartesian x y) = Polar (exercise_6_1_b (x, y)) (Radians (atan2 y x))

fromPolar :: Polar -> Cartesian
fromPolar (Polar r (Radians sig)) = Cartesian (r * cos sig) (r * sin sig)


-- Exercise 6.3

concentric :: (Double -> Picture) -> Int -> Picture
concentric func 1 = func 1
concentric func n = (func (fromIntegral n)) <> (concentric func (n - 1))

-- main :: IO ()
-- main = drawingOf
--  (concentric circle 10)


-- Exercise 6.4
  
type Radians = Double
data Command
 = Forward Double
 | Rotate Radians
 | TeleportTo (Double, Double)
 
runCommands :: [Command] -> Picture
runCommands cmnds = helper 0 0 0 cmnds
  where
    helper :: Double -> Double -> Double -> [Command] -> Picture
    helper _ _ _ [] = blank
    helper _ _ ang (TeleportTo (x_, y_) : cmnds) = helper x_ y_ ang cmnds
    helper x y ang (Rotate ang_ : cmnds) = helper x y (ang + ang_) cmnds
    helper x y ang (Forward dist : cmnds) = (polyline [(x, y), (new_x, new_y)]) <> (helper new_x new_y ang cmnds)
      where 
        new_x = x + (dist * cos ang)
        new_y = y + (dist * sin ang)

-- main = drawingOf (runCommands
--   [ Rotate (2*pi/3), Forward 2, Forward (-4)
--   , TeleportTo (0, 0), Rotate (2*pi/3), Forward 2 ])
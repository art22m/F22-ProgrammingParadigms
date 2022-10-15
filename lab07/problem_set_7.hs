-- {-# OPTIONS_GHC -Wall #-}
-- Murashko Artem SD20-01 --
-- Programming Paradigms Fall 2022 --
-- Problem Set #7 -- 

import Text.Read (readMaybe)
import Data.Char (toUpper)

main :: IO()


-- Exercise 1

guess :: (b -> Bool) -> (String -> IO b) -> IO b
guess p g = do
  s <- getLine
  x <- g s
  case p x of
    True -> return x
    False -> guess p g

-- As we can see from last three rows of a function, p x is a function call, 
-- where p is a function and x is an argument and it's return value is Bool, 
-- thus, p :: (... -> Bool)
-- 
-- getLine call return String, therefore s :: String
-- As we can see g is a function, which takes String as an argument, so,
-- g :: (String -> IO ...). Thus, x :: ...
--
-- Also, return x and guess p g should have the same return type, and it is :: IO ...
-- For now we have, guess :: (... -> Bool) -> (String -> IO ...) -> IO ...
--
-- Since return x and guess p g should be the same, we can name ... in IO ... as a.
-- guess :: (... -> Bool) -> (String -> IO a) -> IO a
--
-- Since x is an argument in p, and return x is equal to IO a, x :: a.
-- guess :: (a -> Bool) -> (String -> IO a) -> IO a
--
-- Answer: guess :: (a -> Bool) -> (String -> IO a) -> IO a


-- Exercise 2

echo :: IO()
echo = do
  str <- getLine
  putStrLn(map toUpper str)
  echo

-- main = echo


-- Exercise 3

-- 3.a

foreverIO :: IO a -> IO b
foreverIO program = do
  program
  foreverIO program

-- main =  foreverIO(putStrLn "Hello!")


-- 3.b

whenIO :: Bool -> IO () -> IO ()
whenIO condition program = do
  case condition of
    True -> program
    False -> return ()

-- main = whenIO False echo


-- 3.c

maybeIO :: Maybe (IO a) -> IO (Maybe a)
maybeIO (Just program) = do
  res <- program
  return (Just res)
maybeIO Nothing = return Nothing


-- 3.d

sequenceMaybeIO :: [IO (Maybe a)] -> IO [a] 
sequenceMaybeIO [] = return []
sequenceMaybeIO (program : programs) = do
  maybeX  <- program          
  xs <- sequenceMaybeIO programs

  case maybeX of 
    Nothing -> return xs
    Just x -> return (x:xs)


-- 3.e

whileJustIO :: (a -> IO (Maybe a)) -> a -> IO ()
whileJustIO function value = do
  maybeResult <- function(value)
  case maybeResult of 
    Nothing -> return ()
    Just result -> whileJustIO function result


-- 3.f

forStateIO_ :: s -> [a] -> (a -> s -> IO s) -> IO s
forStateIO_ state [] function = return state
forStateIO_ state (x:xs) function = do
  newState <- function x state
  forStateIO_ newState xs function


verboseCons :: Int -> [Int] -> IO [Int]
verboseCons x xs = do
  putStrLn ("prepending " ++ show x ++ " to " ++ show xs)
  return (x:xs)

-- forStateIO_ [] [1, 2, 3] verboseCons


-- Exercise 4

iforIO_ :: [s] -> (Int -> s -> IO()) -> IO()
iforIO_ values function = helper 0 values function
  where 
    helper :: Int -> [s] -> (Int -> s -> IO()) -> IO()
    helper _ [] _ = return ()
    helper id (x : xs) function = do
      function id x
      helper (id + 1) xs function

example = do
  iforIO_ [1, 2] (\i n ->
    iforIO_ "ab" (\j c ->
      print ((i, j), replicate n c)))

-- main = example


main = putStrLn "I am at the end"

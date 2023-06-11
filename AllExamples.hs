module Main where

import Data.Functor.Identity
import Data.List (partition)
import System.Environment (getArgs)

main :: IO ()
main = do
  [n] <- getArgs
  print $ example1 (read n)

example1 :: Int -> Bool
example1 n = null [0 .. n]




-- ------------------------------------------------------------------

nullViaEq :: Eq a => [a] -> Bool
nullViaEq xs = xs == []

example2 :: Int -> Bool
example2 n = nullViaEq [0 .. n]

-- ------------------------------------------------------------------

selfEqual :: Eq a => a -> Bool
selfEqual x = x == x

example3 :: Int -> Bool
example3 n = selfEqual [0 .. n]

-- ------------------------------------------------------------------

example4a :: Int -> Bool
example4a n = null (map (<= 10) [0 .. n])

example4b :: Int -> Bool
example4b n = null (reverse [0 .. n])

-- ------------------------------------------------------------------

nullViaLength :: [a] -> Bool
nullViaLength xs = length xs == 0

example5a :: Int -> Bool
example5a n = nullViaLength [0 .. n]

example5b :: Int -> Int
example5b n = length [0 .. n]

length1 :: [a] -> Int
length1 []       = 0
length1 (_ : xs) = 1 + length xs

example5b1 :: Int -> Int
example5b1 n = length1 [0 .. n]

length2 :: [a] -> Int
length2 = lengthAcc2 0

lengthAcc2 :: Int -> [a] -> Int
lengthAcc2 acc []       = acc
lengthAcc2 acc (_ : xs) = lengthAcc2 (1 + acc) xs

example5b2 :: Int -> Int
example5b2 n = length2 [0 .. n]

length3 :: [a] -> Int
length3 = lengthAcc3 0

lengthAcc3 :: Int -> [a] -> Int
lengthAcc3 !acc []       = acc
lengthAcc3 !acc (_ : xs) = lengthAcc3 (1 + acc) xs

example5b3 :: Int -> Int
example5b3 n = length3 [0 .. n]

length4 :: [a] -> Int
length4 = lengthAcc4 0

lengthAcc4 :: Int -> [a] -> Int
lengthAcc4 _   []       = 0
lengthAcc4 acc [_]      = 1 + acc
lengthAcc4 acc (_ : xs) = lengthAcc4 (1 + acc) xs

example5b4 :: Int -> Int
example5b4 n = length4 [0 .. n]

-- ------------------------------------------------------------------

example6 :: Int -> (Int, Int)
example6 n =
  case partition (>= 0) [0 .. n] of
    (xs, ys) -> (sum xs, sum ys)

-- ------------------------------------------------------------------

example7a :: Int -> (Int, Int)
example7a n =
  case partition even [0 .. n] of
    (xs, ys) -> (sum xs, sum ys)

example7b :: Int -> (Int, Int)
example7b n =
  partitionEvenSums [0 .. n]

partitionEvenSums :: [Int] -> (Int, Int)
partitionEvenSums = partitionEvenSumsAcc (0, 0)

partitionEvenSumsAcc :: (Int, Int) -> [Int] -> (Int, Int)
partitionEvenSumsAcc (!x, !y) []       = (x, y)
partitionEvenSumsAcc (!x, !y) (z : zs) =
  if even z
    then partitionEvenSumsAcc (x + z, y) zs
    else partitionEvenSumsAcc (x, y + z) zs

-- ------------------------------------------------------------------

example8a :: Int -> Identity Int
example8a n = length <$> traverse pure [0 .. n]

example8b :: Int -> Maybe Int
example8b n = length <$> traverse pure [0 .. n]

example8c :: Int -> Maybe Int
example8c n =
  traverseLength [0 .. n]

traverseLength :: [a] -> Maybe Int
traverseLength = traverseLengthAcc 0

traverseLengthAcc :: Int -> [a] -> Maybe Int
traverseLengthAcc !acc []       = Just acc
traverseLengthAcc !acc (x : xs) =
  pure x *> traverseLengthAcc (1 + acc) xs

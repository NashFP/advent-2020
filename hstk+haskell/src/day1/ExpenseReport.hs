{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module ExpenseReport where

import           Data.Foldable
import           Data.Maybe
import           Control.Applicative
import           Control.Error
import           Control.Monad

import           Data.Attoparsec.Text

import           Data.IntSet (IntSet)
import qualified Data.IntSet as IS

import           Data.Sequence (Seq(..))
import qualified Data.Sequence as S

import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  i <- input
  part1 $ IS.fromList i
  print $ part2 $ S.fromList i


inputText :: IO Text
inputText = TIO.readFile "./src/day1/input.txt"

parseInput :: Text -> [Int]
parseInput t = let
  parser = many (decimal <* endOfLine)
  res = parseOnly parser t
  in case res of
    Left err -> error $ "Error parsing input file: " <> err
    Right a  -> a

input :: IO [Int]
input = parseInput <$> inputText

-- basically, we're looking for two integers that sum to another known integer: 2000
-- we don't need to n^2 compare all nums against each other, because as soon as we see a single num, we know the other addend we require to sum up to our target
-- given that the puzzle is going to have two numbers which actually do this, we can store the inputs in a collection, subtract later inputs from 2000, then see whether the result exists in our existing collection
-- a set should suffice
-- if it does we solve the puzzle

part1 :: IntSet -> IO ()
part1 set = do
  let z@(a,b) = fromJust $ foldExpenses set
  TIO.putStrLn $ "Got matching expenses: " <> (T.pack $ show z)
  TIO.putStrLn $ "First solution: " <> (T.pack . show $ a * b)

matchingExpense :: Int -> Int
matchingExpense n = 2020 - n

findReciprocalExpense :: Int -> IntSet -> Maybe (Int, Int)
findReciprocalExpense n set =
  case IS.member (matchingExpense n) set of
    False -> Nothing
    True  -> Just (n, matchingExpense n)

foldExpenses :: IntSet -> Maybe (Int, Int)
foldExpenses is = let
  f n maybeSolution = case maybeSolution of
    Just a  -> Just a
    Nothing -> findReciprocalExpense n is
  in IS.foldr f Nothing is

-- so, in part 2, we need to find 3 numbers that sum up to 2020 instead of 2 numbers, then return their product
-- a set may no longer be the correct data structure for holding ints; intsets had only one edge case that would make them unsitable (half of the number, 1010), these can have many
-- so we'll fold, but we'll cut the size of the seq by deleting the processed element from the set we pass to the accumulator
-- if there is no combination between any three elements, we can safely discard the one we match against all other possible combinations as we continue to traverse
-- once we find the solution we no-op through the seq

-- this could've been a list, honestly, but meh, done

type IntSeq = Seq Int
type Solution = (Int, Int, Int)

part2 :: IntSeq -> Either IntSeq Solution
part2 xs = let
  advance :: IntSeq -> IntSeq
  advance = S.drop 1

  innerFold :: Int -> IntSeq -> Maybe Solution
  innerFold n xs = let
    search = do
      a <- xs
      b <- xs
      guard (n + a + b == 2020)
      pure (n, a, b)
    in case search of
        S.Empty        -> Nothing
        solution :<| _ -> Just solution

  outerFold :: Int -> Either IntSeq Solution -> Either IntSeq Solution
  outerFold n eSetSolution = case eSetSolution of
    Right s  -> Right s
    Left seq -> case innerFold n seq of
      Just solution -> Right solution
      Nothing       -> Left $ advance seq

  in foldr outerFold (Left xs) xs

bruteForce :: [Int] -> [Int]
bruteForce xs = do
  a <- xs
  b <- xs
  c <- xs
  guard (a + b + c == 2020)
  pure $ a * b * c 
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RecordWildCards       #-}

module PasswordValidation where

import Debug.Trace

import           Data.Foldable
import           Data.Maybe
import           Control.Applicative
import           Control.Error
import           Control.Monad

import           Data.Attoparsec.Text as AP

import           Data.IntSet (IntSet)
import qualified Data.IntSet as IS

import           Data.Sequence (Seq)
import qualified Data.Sequence as S

import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Text.IO as TIO

-- this is not a hard problem
-- parse data, map validation, print result

main :: IO ()
main = do
  i <- input
  print . length . filter ((==) True) $ fmap isPassValid1 i
  print . length . filter ((==) True) $ fmap isPassValid2 i

inputText :: IO Text
inputText = TIO.readFile "./src/day2/input.txt"

data Password = Password
  { pass :: Text
  , validChar :: Char
  , minOccur :: Int
  , maxOccur :: Int
  } deriving (Eq, Show)

isPassValid1 :: Password -> Bool
isPassValid1 p@Password{..} = let
  occurences = T.count (T.pack $ pure validChar) pass
  in occurences <= maxOccur && occurences >= minOccur

isPassValid2 :: Password -> Bool
isPassValid2 p@Password{..} = let
  len = T.length pass
  xor a b = a && not b || b && not a
  minIndex = minOccur - 1
  maxIndex = maxOccur - 1
  occurence1 = minIndex > len 
    ||  T.index pass minIndex /= validChar
  occurence2 = maxIndex > len
    || T.index pass maxIndex /= validChar
  in xor occurence1 occurence2

parseInput :: Text -> [Password]
parseInput t = let
  parser = many (parsePassword <* endOfLine)
  res = parseOnly parser t
  in case res of
    Left err -> error $ "Error parsing input file: " <> err
    Right a  -> a

parsePassword :: Parser Password
parsePassword = do
  _ <- skipMany space
  minOccur <- decimal
  _ <- char '-'
  maxOccur <- decimal
  _ <- space
  validChar <- anyChar
  _ <- char ':'
  _ <- space
  pass <- T.pack <$> many1 letter

  pure Password{..}

input :: IO [Password]
input = parseInput <$> inputText

{-# LANGUAGE FlexibleInstances #-}

module Day16.Part1 where

import Control.Applicative (Alternative (many), (<|>))
import Control.Arrow (left)
import Control.Monad (guard, replicateM, replicateM_)
import qualified Data.Bits as B
import Data.Bool (bool)
import qualified Data.Char as Char
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (anySingle)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MP

type Parser a = MP.Parsec Void String a

type BinParser a = MP.Parsec Void [Bool] a

hexParser :: Parser [Bool]
hexParser =
  let oneDigit = (flip B.testBit <$> reverse [0 .. 3] <*>) . pure . Char.digitToInt <$> MP.satisfy Char.isHexDigit
   in concat <$> many oneDigit

data Packet = Packet {packetVersion :: Int, packetData :: PacketData}
  deriving (Show)

data PacketData = PacketLiteral Int | PacketOperator Int [Packet]
  deriving (Show)

bitsToInt :: [Bool] -> Int
bitsToInt = go . reverse
  where
    go [] = 0
    go (x : xs) = bool 0 1 x + 2 * go xs

binNumber :: Int -> BinParser Int
binNumber n = bitsToInt <$> replicateM n MP.anySingle

literalData :: BinParser Int
literalData =
  let bitList = do
        recurse <- MP.anySingle
        v <- MP.takeP (pure "bit") 4
        if recurse
          then (v <>) <$> bitList
          else pure v
   in bitsToInt <$> bitList

literalPacketData :: BinParser PacketData
literalPacketData = MP.try $ do
  packetType <- binNumber 3
  guard (packetType == 4)
  PacketLiteral <$> literalData

manyUntil :: Int -> BinParser a -> BinParser [a]
manyUntil off p = do
  curOffset <- MP.getOffset
  if curOffset >= off
    then pure []
    else (:) <$> p <*> manyUntil off p

operatorPacketData :: BinParser PacketData
operatorPacketData = do
  packetType <- binNumber 3
  lengthIsInPackets <- MP.anySingle
  PacketOperator packetType
    <$> if lengthIsInPackets
      then do
        lengthInPackets <- binNumber 11
        replicateM lengthInPackets packetParser
      else do
        lengthInBits <- binNumber 15
        off <- MP.getOffset
        manyUntil (off + lengthInBits) packetParser

packetParser :: BinParser Packet
packetParser = MP.try $ do
  version <- binNumber 3
  body <- literalPacketData <|> operatorPacketData
  return $ Packet version body

tidyErrors :: (MP.TraversableStream s, MP.VisualStream s, MP.ShowErrorComponent e) => Either (MP.ParseErrorBundle s e) a -> Either String a
tidyErrors = left MP.errorBundlePretty

eval :: Packet -> Either String Int
eval (Packet _ (PacketLiteral val)) = pure val
eval (Packet version (PacketOperator 0 subpackets)) = sum <$> traverse eval subpackets
eval (Packet version (PacketOperator 1 subpackets)) = product <$> traverse eval subpackets
eval (Packet version (PacketOperator 2 subpackets)) = minimum <$> traverse eval subpackets
eval (Packet version (PacketOperator 3 subpackets)) = maximum <$> traverse eval subpackets
eval (Packet version (PacketOperator 5 [a, b])) = pure $ bool 0 1 (eval a > eval b)
eval (Packet version (PacketOperator 6 [a, b])) = pure $ bool 0 1 (eval a < eval b)
eval (Packet version (PacketOperator 7 [a, b])) = pure $ bool 0 1 (eval a == eval b)
eval (Packet version (PacketOperator _ _)) = Left "malformed packet"

main :: IO ()
main = do
  contents <- readFile "inputs/day16.txt"
  case MP.parse hexParser "day16" contents of
    Left e -> putStrLn $ MP.errorBundlePretty e
    Right bits -> do
      putStrLn $ bool '0' '1' <$> bits
      case MP.parse packetParser "bits" bits of
        Left e -> putStrLn "failedToParse"
        Right packet -> either putStrLn print $ eval packet
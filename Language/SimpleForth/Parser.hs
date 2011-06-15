
module Language.SimpleForth.Parser where

import Control.Applicative hiding ((<|>))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language

import Language.SimpleForth.Types

baseLanguage = haskell

one :: a -> [a]
one x = [x]

pString :: Parser [StackItem]
pString = one <$> SString <$> stringLiteral baseLanguage

pInteger :: Parser [StackItem]
pInteger = do
  m <- optionMaybe (char '-')
  digits <- many1 digit
  let s = read digits
      n = case m of
            Nothing -> s
            Just _  -> -s
  return [SInteger n]

pWord :: Parser [StackItem]
pWord = do
  word <- many1 (noneOf " \t\r\n")
  let is = case word of
            "NOP"  -> [NOP]
            "DROP" -> [DROP]
            "DUP" ->  [DUP]
            "SWAP" -> [SWAP]
            "." ->    [PRINT]
            ".." ->   [PRINTALL]
            "+" ->    [ADD]
            "-" ->    [SUB]
            "*" ->    [MUL]
            "/" ->    [DIV]
            "REM" ->  [REM]
            "NEG" ->  [NEG]
            "ABS" ->  [ABS]
            ";" ->    [DEFINE]
            ":" ->    [COLON]
            _ ->      [PUSH (SString word), CALL]
  return $ map SInstruction is

pSpaces :: Parser [StackItem]
pSpaces = do
  many1 (oneOf " \t\r\n")
  return []

pForth :: Parser [StackItem]
pForth = do
  ws <- many1 (try pSpaces <|> try pString <|> try pInteger <|> pWord)
  return (concat ws)

parseForth :: FilePath -> String -> Either ParseError Stack
parseForth name str = parse pForth name str

parseForthFile :: FilePath -> IO (Either ParseError Stack)
parseForthFile path = parseFromFile pForth path


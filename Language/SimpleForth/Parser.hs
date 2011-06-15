
module Language.SimpleForth.Parser where

import Control.Applicative hiding ((<|>))
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language

import Language.SimpleForth.Types

baseLanguage = haskell

one :: a -> [a]
one x = [x]

type TParser a = Parsec String Bool a

pString :: TParser [StackItem]
pString = one <$> SString <$> stringLiteral baseLanguage

pInteger :: TParser [StackItem]
pInteger = do
  m <- optionMaybe (char '-')
  digits <- many1 digit
  let s = read digits
      n = case m of
            Nothing -> s
            Just _  -> -s
  return [SInteger n]

instr :: Instruction -> TParser [StackItem]
instr i = do
  defMode <- getState
  if defMode
    then return [Quote (SInstruction i)]
    else return [SInstruction i]

pWord :: TParser [StackItem]
pWord = do
  word <- many1 (noneOf " \t\r\n")
  case word of
    "NOP"  -> instr NOP
    "DROP" -> instr DROP
    "DUP" ->  instr DUP
    "SWAP" -> instr SWAP
    "." ->    instr PRINT
    ".." ->   instr PRINTALL
    "+" ->    instr ADD
    "-" ->    instr SUB
    "*" ->    instr MUL
    "/" ->    instr DIV
    "REM" ->  instr REM
    "NEG" ->  instr NEG
    "ABS" ->  instr ABS
    ";" ->    putState False >> return [SInstruction DEFINE]
    ":" ->    putState True >> return []
    _ -> do
         defMode <- getState
         if defMode
           then return [SString word]
           else return [SString word, SInstruction CALL]

pSpaces :: TParser [StackItem]
pSpaces = do
  many1 (oneOf " \t\r\n")
  return []

pForth :: TParser [StackItem]
pForth = do
  ws <- many1 (try pSpaces <|> try pString <|> try pInteger <|> pWord)
  return (concat ws)

parseForth :: FilePath -> String -> Either ParseError Stack
parseForth name str = runParser pForth False name str

parseForthFile :: FilePath -> IO (Either ParseError Stack)
parseForthFile path = do
  str <- readFile path
  return $ runParser pForth False path str



module Language.SimpleForth.Parser where

import Control.Applicative hiding ((<|>))
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language

import Language.SimpleForth.Types

baseLanguage = haskell

data ParserState = PState {
  inDefinition :: Bool,
  newWord :: Bool }
  deriving (Eq, Show)

emptyState = PState {
  inDefinition = False,
  newWord = False }

type TParser a = Parsec String ParserState a

pString :: TParser [StackItem]
pString = do
  st <- getState
  str <- SString <$> stringLiteral baseLanguage
  if inDefinition st
    then return [Quote str]
    else return [str]

pInteger :: TParser [StackItem]
pInteger = do
  m <- optionMaybe (char '-')
  digits <- many1 digit
  let s = read digits
      n = case m of
            Nothing -> s
            Just _  -> -s
  st <- getState
  if inDefinition st
    then return [Quote $ SInteger n]
    else return [SInteger n]

instr :: Instruction -> TParser [StackItem]
instr i = do
  st <- getState
  if inDefinition st
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
    ";" ->    putState (PState False False) >> return [SInstruction DEFINE]
    ":" ->    putState (PState True True) >> return []
    _ -> do
         st <- getState
         if newWord st
           then do
                putState $ st {newWord = False}
                return [SString word]
           else if inDefinition st
                  then return [Quote $ SInstruction $ CALL word]
                  else return [SInstruction $ CALL word]

pSpaces :: TParser [StackItem]
pSpaces = do
  many1 (oneOf " \t\r\n")
  return []

pForth :: TParser [StackItem]
pForth = do
  ws <- many1 (try pSpaces <|> try pString <|> try pInteger <|> pWord)
  return (concat ws)

parseForth :: FilePath -> String -> Either ParseError Stack
parseForth name str = runParser pForth emptyState name str

parseForthFile :: FilePath -> IO (Either ParseError Stack)
parseForthFile path = do
  str <- readFile path
  return $ runParser pForth emptyState path str


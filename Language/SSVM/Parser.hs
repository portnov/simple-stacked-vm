
module Language.SSVM.Parser
  (parseVM, parseSourceFile) where

import Data.Monoid
import qualified Data.Map as M
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language

import Language.SSVM.Types

baseLanguage = haskell

data ParserState = PState {
  inDefinition :: Bool,
  newWord :: Bool,
  wordsCounter :: Int }
  deriving (Eq, Show)

emptyState = PState {
  inDefinition = False,
  newWord = False,
  wordsCounter = 0 }

type TParser a = Parsec String ParserState a

code :: [StackItem] -> TParser Code
code list = return $ Code [M.empty] list

startDefinition :: TParser ()
startDefinition = do
  st <- getState
  putState $ st {inDefinition = True, newWord = True}

endDefinition :: TParser ()
endDefinition = do
  st <- getState
  putState $ st {inDefinition = False, newWord = False}

pString :: TParser Code
pString = do
  st <- getState
  str <- stringLiteral baseLanguage
  if newWord st
    then do
         putState $ st {newWord = False}
         code [SString str]
    else if inDefinition st
           then code [Quote $ SString str]
           else code [SString str]

pInteger :: TParser Code
pInteger = do
  m <- optionMaybe (char '-')
  digits <- many1 digit
  let s = read digits
      n = case m of
            Nothing -> s
            Just _  -> -s
  st <- getState
  if inDefinition st
    then code [Quote $ SInteger n]
    else code [SInteger n]

addMark :: String -> TParser Code
addMark name = do
  st <- getState
  let addr = wordsCounter st
  return $ Code [M.singleton name addr] []

instr :: Instruction -> TParser Code
instr i = do
  st <- getState
  if inDefinition st
    then code [Quote (SInstruction i)]
    else code [SInstruction i]

pWord :: TParser Code
pWord = do
  word <- many1 (noneOf " \t\r\n")
  case word of
    "NOP"  -> instr NOP
    "DROP" -> instr DROP
    "DUP" ->  instr DUP
    "SWAP" -> instr SWAP
    "OVER" -> instr OVER
    "." ->    instr PRINT
    ".." ->   instr PRINTALL
    "+" ->    instr ADD
    "-" ->    instr SUB
    "*" ->    instr MUL
    "/" ->    instr DIV
    "REM" ->  instr REM
    "NEG" ->  instr NEG
    "ABS" ->  instr ABS
    "CMP" ->  instr CMP
    ";" ->    endDefinition >> code [SInstruction DEFINE]
    ":" ->    startDefinition >> code [SInstruction COLON]
    "VARIABLE" -> endDefinition >> code [SInstruction VARIABLE]
    "!" ->    instr ASSIGN
    "@" ->    instr READ
    "INPUT" -> instr INPUT
    "MARK" -> instr MARK
    "GOTO" -> instr GOTO
    "JZ"   -> instr JZ
    "JNZ"  -> instr JNZ
    "JGT"  -> instr JGT
    "JLT"  -> instr JLT
    "JGE"  -> instr JGE
    "JLE"  -> instr JLE
    _ | head word == '@' -> instr (GETMARK $ tail word)
      | otherwise -> do
         st <- getState
         if newWord st
           then do
                putState $ st {newWord = False}
                code [SString word]
           else if inDefinition st
                  then code [Quote $ SInstruction $ CALL word]
                  else code [SInstruction $ CALL word]

pLabel :: TParser Code
pLabel = do
  char '.'
  name <- many1 (noneOf ". \t\r\n")
  addMark name

step :: Int -> TParser ()
step k = do
  st <- getState
  putState $ st {wordsCounter = k + wordsCounter st}

pSpaces :: TParser Code
pSpaces = do
  many1 (oneOf " \t\r\n")
  code []

pSource :: TParser Code
pSource = do
    ws <- many1 anyWord
    return (mconcat ws)
  where
    anyWord = do
      word <- (try pSpaces <|> try pString <|> try pInteger <|> try pLabel <|> pWord)
      step (length $ cCode word)
      return word

parseVM :: FilePath -> String -> Either ParseError Code
parseVM name str = runParser pSource emptyState name str

parseSourceFile :: FilePath -> IO (Either ParseError Code)
parseSourceFile path = do
  str <- readFile path
  return $ runParser pSource emptyState path str


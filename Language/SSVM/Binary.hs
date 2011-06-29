{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Language.SSVM.Binary
  (dumpCode, loadCode)
  where

import Control.Applicative
import Control.Monad (forM_)
import qualified Control.Monad.State as S
import Data.BinaryState
import qualified Data.Map as M
import Data.Char
import Data.Word

import Language.SSVM.Types

data BState = BState {
  bMarks :: Marks,
  bWords :: M.Map String Int,
  bLastWord :: Int,
  bAfterColon :: Bool }
  deriving (Eq, Show)

emptyBState :: BState
emptyBState = BState {
  bMarks = M.empty,
  bWords = M.empty,
  bLastWord = 0,
  bAfterColon = False }

type Put a = PutState BState a
type Get a = GetState BState a

allocWord :: String -> Put Int
allocWord w = do
  st <- S.get
  let next = 1 + bLastWord st
      ws = M.insert w next (bWords st)
  S.put $ st {bWords = ws, bLastWord = next}
  return next

getWordN :: String -> Put Int
getWordN w = do
  ws <- S.gets bWords
  case M.lookup w ws of
    Nothing -> fail $ "Undefined word: " ++ w
    Just i -> return i

byte :: Word8 -> Put ()
byte x = putZ x

char :: Char -> Put ()
char c = putZ (fromIntegral (ord c) :: Word8)

getChar8 :: Get Char
getChar8 = (chr . fromIntegral) <$> (getZ :: Get Word8)

getMark :: String -> Put Int
getMark name = do
  ms <- S.gets bMarks
  case M.lookup name ms of
    Nothing -> fail $ "Undefined mark: @" ++ name
    Just n  -> return n

wordName :: Int -> Get String
wordName n = return $ "WORD_" ++ show n

markName :: Int -> Get String
markName n = do
  let name = "mark_at_" ++ show n
  st <- S.get
  let ms = M.insert name n (bMarks st)
  S.put $ st {bMarks = ms}
  return name

instance BinaryState BState Instruction where
  put NOP      = byte 0
  put (PUSH x) = byte 1 >> put x
  put DROP     = byte 2
  put DUP      = byte 3
  put SWAP     = byte 4
  put OVER     = byte 5
  put PRINT    = byte 6
  put PRINTALL = byte 7
  put ADD      = byte 8
  put MUL      = byte 9
  put DIV      = byte 10
  put REM      = byte 11
  put SUB      = byte 12
  put NEG      = byte 13
  put ABS      = byte 14
  put CMP      = byte 15
  put DEFINE   = byte 16
  put COLON    = do
                 st <- S.get
                 S.put $ st {bAfterColon = True}
                 byte 17
  put (CALL s) = do
                 n <- getWordN s
                 byte 18
                 putZ n
  put VARIABLE = byte 19
  put ASSIGN   = byte 20
  put READ     = byte 21
  put INPUT    = byte 22
  put MARK     = byte 23
  put (GETMARK x) = do
                    n <- getMark x
                    byte 24
                    putZ n
  put GOTO     = byte 25
  put JZ       = byte 26
  put JNZ      = byte 27
  put JGT      = byte 28
  put JLT      = byte 29
  put JGE      = byte 30
  put JLE      = byte 31
  put ARRAY    = byte 32
  put READ_ARRAY   = byte 33
  put ASSIGN_ARRAY = byte 34

  get = do
    c <- getZ :: Get Word8
    case c of
      0 -> return NOP
      1 -> PUSH <$> get
      2 -> return DROP
      3 -> return DUP
      4 -> return SWAP
      5 -> return OVER
      6 -> return PRINT
      7 -> return PRINTALL
      8 -> return ADD
      9 -> return MUL
      10 -> return DIV
      11 -> return REM
      12 -> return SUB
      13 -> return NEG
      14 -> return ABS
      15 -> return CMP
      16 -> return DEFINE
      17 -> return COLON
      18 -> CALL <$> (wordName =<< getZ)
      19 -> return VARIABLE
      20 -> return ASSIGN
      21 -> return READ
      22 -> return INPUT
      23 -> return MARK
      24 -> GETMARK <$> (markName =<< getZ)
      25 -> return GOTO
      26 -> return JZ
      27 -> return JNZ
      28 -> return JGT
      29 -> return JLT
      30 -> return JGE
      31 -> return JLE
      32 -> return ARRAY
      33 -> return READ_ARRAY
      34 -> return ASSIGN_ARRAY
      _ -> fail $ "Unknown opcode: " ++ show c

instance BinaryState BState StackItem where
  put (SInteger x)     = putZ 'I' >> putZ x
  put (SString x)      = do
                         a <- S.gets bAfterColon
                         if a
                           then do
                                st <- S.get
                                S.put $ st {bAfterColon = False}
                                putZ 'W'
                                w <- allocWord x
                                putZ w
                            else putZ 'S' >> putZ x
  put (SInstruction x) = putZ 'O' >> put x
  put (SArray _)       = fail "Array literals are not supported"
  put (Quote x)        = putZ 'Q' >> put x

  get = do
    c <- getChar8
    case c of
      'I' -> SInteger <$> getZ
      'S' -> SString <$> getZ
      'O' -> SInstruction <$> get
      'Q' -> Quote <$> get
      'W' -> SString <$> (wordName =<< getZ)
      _   -> fail $ "Unknown stack item type: " ++ [c]

instance BinaryState BState [StackItem] where
  put list = forM_ list put

  get = getUntilEOF
    where
      getUntilEOF = do
        b <- isEmpty
        if b
          then return []
          else do
               x <- get
               next <- getUntilEOF
               return (x:next)

-- | Dump bytecode to file
dumpCode :: FilePath -> Code -> IO ()
dumpCode path (Code marks code) = encodeFile path (emptyBState {bMarks = head marks}) code

-- | Load bytecode from file
loadCode :: FilePath -> IO Code
loadCode path = do
  (code, st) <- decodeFile' path emptyBState
  return $ Code [bMarks st] code


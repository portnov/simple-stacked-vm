
module Language.SimpleForth.Binary where

import Control.Applicative
import Data.Binary
import Data.Char

import Language.SimpleForth.Types

byte :: Word8 -> Put
byte x = putWord8 x

char :: Char -> Put
char c = putWord8 (fromIntegral $ ord c)

getChar8 :: Get Char
getChar8 = (chr . fromIntegral) <$> getWord8

instance Binary Instruction where
  put NOP      = byte 0
  put (PUSH x) = byte 1 >> put x
  put DROP     = byte 2
  put DUP      = byte 3
  put SWAP     = byte 4
  put PRINT    = byte 5
  put PRINTALL = byte 6
  put ADD      = byte 7
  put MUL      = byte 8
  put DIV      = byte 9
  put REM      = byte 10
  put SUB      = byte 11
  put NEG      = byte 12
  put ABS      = byte 13
  put DEFINE   = byte 14
  put COLON    = byte 15
  put CALL     = byte 16

  get = do
    c <- getWord8
    case c of
      0 -> return NOP
      1 -> PUSH <$> get
      2 -> return DROP
      3 -> return DUP
      4 -> return SWAP
      5 -> return PRINT
      6 -> return PRINTALL
      7 -> return ADD
      8 -> return MUL
      9 -> return DIV
      10 -> return REM
      11 -> return SUB
      12 -> return NEG
      13 -> return ABS
      14 -> return DEFINE
      15 -> return COLON
      16 -> return CALL
      _ -> fail $ "Unknown opcode: " ++ show c

instance Binary StackItem where
  put (SInteger x)     = char 'I' >> put x
  put (SString x)      = char 'S' >> put x
  put (SInstruction x) = char 'O' >> put x

  get = do
    c <- getChar8
    case c of
      'I' -> SInteger <$> get
      'S' -> SString <$> get
      'O' -> SInstruction <$> get


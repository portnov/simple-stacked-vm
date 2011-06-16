
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
  put DEFINE   = byte 15
  put COLON    = byte 16
  put (CALL s) = byte 17 >> put s
  put VARIABLE = byte 18
  put ASSIGN   = byte 19
  put READ     = byte 20
  put INPUT    = byte 21

  get = do
    c <- getWord8
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
      15 -> return DEFINE
      16 -> return COLON
      17 -> CALL <$> get
      18 -> return VARIABLE
      19 -> return ASSIGN
      20 -> return READ
      21 -> return INPUT
      _ -> fail $ "Unknown opcode: " ++ show c

instance Binary StackItem where
  put (SInteger x)     = put 'I' >> put x
  put (SString x)      = put 'S' >> put x
  put (SInstruction x) = put 'O' >> put x
  put (Quote x)        = put 'Q' >> put x

  get = do
    c <- getChar8
    case c of
      'I' -> SInteger <$> get
      'S' -> SString <$> get
      'O' -> SInstruction <$> get
      'Q' -> Quote <$> get


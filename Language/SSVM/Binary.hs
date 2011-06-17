
module Language.SSVM.Binary where

import Control.Applicative
import Data.Binary
import Data.Char

import Language.SSVM.Types

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
  put CMP      = byte 15
  put DEFINE   = byte 16
  put COLON    = byte 17
  put (CALL s) = byte 18 >> put s
  put VARIABLE = byte 19
  put ASSIGN   = byte 20
  put READ     = byte 21
  put INPUT    = byte 22
  put MARK     = byte 23
  put (GETMARK x) = byte 24 >> put x
  put GOTO     = byte 25
  put JZ       = byte 26
  put JNZ      = byte 27
  put JGT      = byte 28
  put JLT      = byte 29
  put JGE      = byte 30
  put JLE      = byte 31

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
      15 -> return CMP
      16 -> return DEFINE
      17 -> return COLON
      18 -> CALL <$> get
      19 -> return VARIABLE
      20 -> return ASSIGN
      21 -> return READ
      22 -> return INPUT
      23 -> return MARK
      24 -> GETMARK <$> get
      25 -> return GOTO
      26 -> return JZ
      27 -> return JNZ
      28 -> return JGT
      29 -> return JLT
      30 -> return JGE
      31 -> return JLE
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

instance Binary Code where
  put (Code marks code) = put marks >> put code

  get = Code <$> get <*> get


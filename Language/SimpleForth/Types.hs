{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances #-}
module Language.SimpleForth.Types where

import Control.Monad
import Control.Monad.State
import Data.Data
import qualified Data.Map as M

data StackItem =
    SInteger Integer
  | SString String
  | SInstruction Instruction
  | Quote StackItem
  deriving (Eq, Show, Data, Typeable)

showType :: StackItem -> String
showType x = show (toConstr x)

showItem :: StackItem -> String
showItem (SInteger x) = show x
showItem (SString x) = x
showItem (SInstruction x) = show x
showItem (Quote x) = "'" ++ show x

type Stack = [StackItem]

class (Data a, Typeable a) => StackType a where
  toStack :: a -> StackItem
  fromStack :: StackItem -> Maybe a

instance StackType Integer where
  toStack = SInteger

  fromStack (SInteger x) = Just x
  fromStack _ = Nothing

instance StackType String where
  toStack = SString

  fromStack (SString x) = Just x
  fromStack _ = Nothing

instance StackType Instruction where
  toStack = SInstruction

  fromStack (SInstruction x) = Just x
  fromStack _ = Nothing

data Instruction =
    NOP
  | PUSH StackItem
  | DROP
  | DUP
  | SWAP
  | OVER
  | PRINT
  | PRINTALL
  | ADD
  | MUL
  | DIV
  | REM
  | SUB
  | NEG
  | ABS
  | DEFINE
  | COLON
  | CALL String
  | VARIABLE
  | ASSIGN
  | READ
  deriving (Eq, Data, Typeable)

instance Show Instruction where
  show NOP = "NOP"
  show (PUSH x) = show x
  show DROP     = "DROP"
  show DUP      = "DUP"
  show SWAP     = "SWAP"
  show OVER     = "OVER"
  show PRINT    = "."
  show PRINTALL = ".."
  show ADD      = "+"
  show MUL      = "*"
  show DIV      = "/"
  show REM      = "REM"
  show SUB      = "-"
  show NEG      = "NEG"
  show ABS      = "ABS"
  show DEFINE   = ";"
  show COLON    = ":"
  show (CALL s) = "<CALL " ++ s ++ ">"
  show VARIABLE = "VARIABLE"
  show ASSIGN   = "!"
  show READ     = "@"

data VMState = VMState {
  vmStack :: Stack,
  vmCurrentDefinition :: Stack,
  vmDefinitions :: M.Map String [StackItem],
  vmVariables :: M.Map Int StackItem,
  vmNextVariable :: Int
  }
  deriving (Eq, Show)

emptyVMState :: VMState
emptyVMState = VMState {
  vmStack = [],
  vmCurrentDefinition = [],
  vmDefinitions = M.empty,
  vmVariables = M.empty,
  vmNextVariable = 0 }

type Forth a = StateT VMState IO a


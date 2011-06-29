{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances #-}
module Language.SSVM.Types where

import Control.Monad.State
import Data.Data
import Data.Monoid
import Data.Array
import qualified Data.Map as M

type DArray = Array Int StackItem

-- | Stack item
data StackItem =
    SInteger Integer
  | SString String
  | SInstruction Instruction
  | SArray DArray
  | Quote StackItem
  deriving (Eq, Data, Typeable)

-- | Show type of item
showType :: StackItem -> String
showType x = show (toConstr x)

instance Show StackItem where
  show (SInteger x) = show x
  show (SString s)  = show s
  show (SArray a)   = "[" ++ unwords (map show $ elems a) ++ "]"
  show (SInstruction i) = show i
  show (Quote x)    = show x

showItem :: StackItem -> String
showItem = show

showPrint :: StackItem -> String
showPrint (SInteger x) = show x
showPrint (SString s)  = s
showPrint (SArray a)   = "[" ++ unwords (map show $ elems a) ++ "]"
showPrint (SInstruction i) = show i
showPrint (Quote x)    = show x

showCode :: Code -> String
showCode (Code marks code) =
    unwords $  zipWith (showOne $ head marks) [1..] code
  where
    showOne ms n item = showItem item ++ concatMap (" ."++) (lookupRev n $ M.assocs ms)

    lookupRev _ [] = []
    lookupRev key ((v,k):other) 
                  | key == k  = v: lookupRev key other
                  | otherwise = lookupRev key other
    
type Stack = [StackItem]

type Marks = M.Map String Int

showMarks :: Marks -> String
showMarks ms = unlines $ map s $ M.assocs ms
  where
    s (k,v) = "\t" ++ k ++ ": " ++ show v

-- | VM code
data Code = Code {
  cMarks :: [Marks],   -- ^ marks stack
  cCode :: [StackItem] }
  deriving (Eq, Show, Data, Typeable)

instance Monoid Code where
  mempty = Code [M.empty] []
  mappend (Code l1 c1) (Code l2 c2) = Code (M.union m1 m2: ms) (c1 ++ c2)
    where
      (m1:ms) = l1
      (m2:_)  = l2

class (Data a, Typeable a) => StackType a where
  toStack :: a -> StackItem
  fromStack :: StackItem -> Maybe a

instance StackType Integer where
  toStack = SInteger

  fromStack (SInteger x) = Just x
  fromStack _ = Nothing

instance StackType Int where
  toStack = SInteger . fromIntegral

  fromStack (SInteger x) = Just (fromIntegral x)
  fromStack _            = Nothing

instance StackType String where
  toStack = SString

  fromStack (SString x) = Just x
  fromStack _ = Nothing

instance StackType Instruction where
  toStack = SInstruction

  fromStack (SInstruction x) = Just x
  fromStack _ = Nothing

instance StackType DArray where
  toStack = SArray

  fromStack (SArray a) = Just a
  fromStack _          = Nothing

-- | VM instructions
data Instruction =
    NOP            -- ^ Do nothing
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
  | CMP
  | DEFINE
  | COLON
  | CALL String     -- ^ Call named user-defined word
  | VARIABLE
  | ASSIGN
  | READ
  | INPUT
  | MARK
  | GETMARK String
  | GOTO
  | JZ
  | JNZ
  | JGT
  | JLT
  | JGE
  | JLE
  | ARRAY
  | READ_ARRAY
  | ASSIGN_ARRAY
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
  show CMP      = "CMP"
  show DEFINE   = ";"
  show COLON    = ":"
  show (CALL s) = s
  show VARIABLE = "VARIABLE"
  show ASSIGN   = "!"
  show READ     = "@"
  show INPUT    = "INPUT"
  show MARK     = "MARK"
  show (GETMARK x) = "@" ++ x
  show GOTO     = "GOTO"
  show JZ       = "JZ"
  show JNZ      = "JNZ"
  show JGT      = "JGT"
  show JLT      = "JLT"
  show JGE      = "JGE"
  show JLE      = "JLE"
  show ARRAY    = "ARRAY"
  show READ_ARRAY   = "[@]"
  show ASSIGN_ARRAY = "[!]"

-- | Word definition
data Definition = Definition Int Stack
  deriving (Eq, Show)

-- | VM state
data VMState = VMState {
  vmStack :: Stack,                         -- ^ current VM stack
  vmCurrentDefinition :: Stack,             -- ^ current definition
  vmDefinitions :: M.Map String Definition, -- ^ already defined words
  vmVariables :: M.Map Int StackItem,       -- ^ variables values
  vmNextVariable :: Int,                    -- ^ next variable number
  vmPC :: Int,                              -- ^ program counter
  vmTraceMode :: Bool                       -- ^ trace mode
  }
  deriving (Eq, Show)

-- | Starting VM state
emptyVMState :: VMState
emptyVMState = VMState {
  vmStack = [],
  vmCurrentDefinition = [],
  vmDefinitions = M.empty,
  vmVariables = M.empty,
  vmNextVariable = 0,
  vmPC = 0,
  vmTraceMode = False }

-- | VM monad
type VM a = StateT VMState IO a


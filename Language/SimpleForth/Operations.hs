{-# LANGUAGE ScopedTypeVariables #-}
module Language.SimpleForth.Operations where

import Data.Data
import Data.Char
import qualified Data.Map as M
import Data.Array
import Control.Monad.State

import Language.SimpleForth.Types

step :: Forth ()
step = do
  st <- get
  let was = vmPC st
  put $ st {vmPC = was + 1}

withStack :: (Stack -> Stack) -> Forth ()
withStack fn = do
  st <- get
  let stk = vmStack st
  setStack (fn stk)

withStackM :: (Stack -> Forth Stack) -> Forth ()
withStackM fn = do
  st <- get
  let stk = vmStack st
  stk' <- fn stk
  setStack stk'

setStack :: Stack -> Forth ()
setStack stk = do
  st <- get
  put $ st { vmStack = stk }

push :: (StackType a) => a -> Forth ()
push x = withStack (toStack x:)

pushS :: StackItem -> Forth ()
pushS x = withStack (x:)

pushD :: StackItem -> Forth ()
pushD x = do
  st <- get
  let def = vmCurrentDefinition st
  put $ st {vmCurrentDefinition = (x:def)}

endDef :: Forth ()
endDef = do
  st <- get
  put $ st {vmCurrentDefinition = []}

pop :: Forth ()
pop = withStackM pop'
  where
    pop' [] = fail "DROP on empty stack!"
    pop' (x:xs) = return xs

dup :: Forth ()
dup = withStackM dup'
  where
    dup' [] = fail "DUP on empty stack!"
    dup' (x:xs) = return (x:x:xs)

swap :: Forth ()
swap = withStackM swap'
  where
    swap' [] = fail "SWAP on empty stack!"
    swap' [_] = fail "SWAP on single-element stack!"
    swap' (x:y:xs) = return (y:x:xs)

over :: Forth ()
over = withStackM over'
  where
    over' [] = fail "OVER on empty stack!"
    over' [_] = fail "OVER on single-element stack!"
    over' (x:y:xs) = return (y:x:y:xs)

printStack :: Forth ()
printStack = do
  stk <- gets vmStack
  lift $ putStrLn $ unwords $ map showItem stk

printCurrentDef :: Forth ()
printCurrentDef = do
  def <- gets vmCurrentDefinition
  lift $ putStr "Current definition: "
  lift $ putStrLn $ unwords $ map showItem (reverse def)

getStack :: Forth StackItem
getStack = do
  stk <- gets vmStack
  case stk of
    [] -> fail "Trying to get element from empty stack!"
    (x:xs) -> do
              setStack xs
              return x

getArg :: forall a. (StackType a) => Forth a
getArg = do
  stk <- gets vmStack
  case stk of
    [] -> fail "Trying to get element from empty stack!"
    (x:xs) -> case fromStack x of
                Just r -> do
                          setStack xs
                          return r
                Nothing -> fail $ "Stack type error: got " ++ showType x ++
                                  " while expecting " ++ show (typeOf (undefined :: a))

liftF :: (StackType a) => (a -> a) -> Forth ()
liftF fn = do
  x <- getArg
  push (fn x)

liftF2 :: (StackType a) => (a -> a -> a) -> Forth ()
liftF2 op = do
  y <- getArg
  x <- getArg
  let result = x `op` y
  push result

add :: Forth ()
add = liftF2 ((+) :: Integer -> Integer -> Integer)

sub :: Forth ()
sub = liftF2 ((-) :: Integer -> Integer -> Integer)

neg :: Forth ()
neg = liftF ((\x -> -x) :: Integer -> Integer)

absF :: Forth ()
absF = liftF (abs :: Integer -> Integer)

mul :: Forth ()
mul = liftF2 ((*) :: Integer -> Integer -> Integer)

divide :: Forth ()
divide = liftF2 (div :: Integer -> Integer -> Integer)

remF :: Forth ()
remF = liftF2 (mod :: Integer -> Integer -> Integer)

cmpF :: Forth ()
cmpF = do
    y <- getStack
    x <- getStack
    case (x,y) of
      (SInteger a, SInteger b) -> push (cmp a b)
      (SString a, SString b) -> push (cmp a b)
  where
    cmp :: (Ord a) => a -> a -> Integer
    cmp a b = case compare a b of
               LT -> -1
               EQ -> 0
               GT -> 1

printF :: Forth ()
printF = do
  x <- getStack
  lift $ putStr $ showItem x

define :: Forth ()
define = do
    ws <- gets vmCurrentDefinition
    endDef
    w <- getStack
    case w of
      SString name -> do
        st <- get
        dict <- gets vmDefinitions
        pc <- gets vmPC
        let start = pc - length ws
            dict' = M.insert name (Definition start $ reverse ws) dict
        put $ st {vmDefinitions = dict'}
      x -> fail $ "New word name is " ++ showType x ++ ", not String!"

recall :: String -> Forth Definition
recall name = do
  dict <- gets vmDefinitions
  case M.lookup name dict of
    Nothing -> fail $ "Unknown word: " ++ name
    Just list -> return list

variable :: Forth ()
variable = do
  name <- getArg
  st <- get
  pc <- gets vmPC
  let n = vmNextVariable st
      dict = M.insert name (Definition (pc-1) [SInteger $ fromIntegral n]) (vmDefinitions st)
  put $ st {vmDefinitions = dict, vmNextVariable = n+1}

assign :: Forth ()
assign = do
  n <- getArg :: Forth Integer
  value <- getStack
  st <- get
  let vars = M.insert (fromIntegral n) value (vmVariables st)
  put $ st {vmVariables = vars}

readVar :: Forth ()
readVar = do
  n <- getArg :: Forth Integer
  vars <- gets vmVariables
  case M.lookup (fromIntegral n) vars of
    Nothing -> fail $ "Trying to read variable before assignment: #" ++ show n
    Just value -> pushS value

input :: Forth ()
input = do
  str <- lift getLine
  if all isDigit str
    then pushS (SInteger $ read str)
    else pushS (SString str)

mark :: Forth ()
mark = do
  pc <- gets vmPC
  pushS (SInteger $ fromIntegral pc)

branch :: Int -> Forth ()
branch n = do
  st <- get
  put $ st {vmPC = n}

goto :: Forth ()
goto = do
  n <- getArg :: Forth Integer
  branch (fromIntegral n)

jumpIf :: (Integer -> Bool) -> Forth ()
jumpIf test = do
  addr <- getArg :: Forth Integer
  cond <- getStack
  case cond of
    SInteger i -> if test i
                    then branch (fromIntegral addr)
                    else step
    _ -> fail $ "Condition value is " ++ showType cond ++ ", not Integer!"

getMark :: Marks -> String -> Forth ()
getMark marks name = do
  case M.lookup name marks of
    Just x -> pushS (SInteger $ fromIntegral x)
    Nothing -> fail $ "Undefined mark: " ++ name

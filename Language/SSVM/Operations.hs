{-# LANGUAGE ScopedTypeVariables #-}
module Language.SSVM.Operations where

import Data.Data
import Data.Char
import qualified Data.Map as M
import Data.Array
import Control.Monad.State

import Language.SSVM.Types

step :: VM ()
step = do
  st <- get
  let was = vmPC st
  put $ st {vmPC = was + 1}

withStack :: (Stack -> Stack) -> VM ()
withStack fn = do
  st <- get
  let stk = vmStack st
  setStack (fn stk)

withStackM :: (Stack -> VM Stack) -> VM ()
withStackM fn = do
  st <- get
  let stk = vmStack st
  stk' <- fn stk
  setStack stk'

setStack :: Stack -> VM ()
setStack stk = do
  st <- get
  put $ st { vmStack = stk }

push :: (StackType a) => a -> VM ()
push x = withStack (toStack x:)

pushS :: StackItem -> VM ()
pushS x = withStack (x:)

pushD :: StackItem -> VM ()
pushD x = do
  st <- get
  let def = vmCurrentDefinition st
  put $ st {vmCurrentDefinition = (x:def)}

endDef :: VM ()
endDef = do
  st <- get
  put $ st {vmCurrentDefinition = []}

pop :: VM ()
pop = withStackM pop'
  where
    pop' [] = fail "DROP on empty stack!"
    pop' (x:xs) = return xs

dup :: VM ()
dup = withStackM dup'
  where
    dup' [] = fail "DUP on empty stack!"
    dup' (x:xs) = return (x:x:xs)

swap :: VM ()
swap = withStackM swap'
  where
    swap' [] = fail "SWAP on empty stack!"
    swap' [_] = fail "SWAP on single-element stack!"
    swap' (x:y:xs) = return (y:x:xs)

over :: VM ()
over = withStackM over'
  where
    over' [] = fail "OVER on empty stack!"
    over' [_] = fail "OVER on single-element stack!"
    over' (x:y:xs) = return (y:x:y:xs)

printStack :: VM ()
printStack = do
  stk <- gets vmStack
  lift $ putStrLn $ unwords $ map showItem stk

printCurrentDef :: VM ()
printCurrentDef = do
  def <- gets vmCurrentDefinition
  lift $ putStr "Current definition: "
  lift $ putStrLn $ unwords $ map showItem (reverse def)

getStack :: VM StackItem
getStack = do
  stk <- gets vmStack
  case stk of
    [] -> fail "Trying to get element from empty stack!"
    (x:xs) -> do
              setStack xs
              return x

getArg :: forall a. (StackType a) => VM a
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

liftF :: (StackType a) => (a -> a) -> VM ()
liftF fn = do
  x <- getArg
  push (fn x)

liftF2 :: (StackType a) => (a -> a -> a) -> VM ()
liftF2 op = do
  y <- getArg
  x <- getArg
  let result = x `op` y
  push result

add :: VM ()
add = liftF2 ((+) :: Integer -> Integer -> Integer)

sub :: VM ()
sub = liftF2 ((-) :: Integer -> Integer -> Integer)

neg :: VM ()
neg = liftF ((\x -> -x) :: Integer -> Integer)

absF :: VM ()
absF = liftF (abs :: Integer -> Integer)

mul :: VM ()
mul = liftF2 ((*) :: Integer -> Integer -> Integer)

divide :: VM ()
divide = liftF2 (div :: Integer -> Integer -> Integer)

remF :: VM ()
remF = liftF2 (mod :: Integer -> Integer -> Integer)

cmpF :: VM ()
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

printF :: VM ()
printF = do
  x <- getStack
  lift $ putStr $ showItem x

define :: VM ()
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

recall :: String -> VM Definition
recall name = do
  dict <- gets vmDefinitions
  case M.lookup name dict of
    Nothing -> fail $ "Unknown word: " ++ name
    Just list -> return list

variable :: VM ()
variable = do
  name <- getArg
  st <- get
  pc <- gets vmPC
  let n = vmNextVariable st
      dict = M.insert name (Definition (pc-1) [SInteger $ fromIntegral n]) (vmDefinitions st)
  put $ st {vmDefinitions = dict, vmNextVariable = n+1}

assign :: VM ()
assign = do
  n <- getArg :: VM Integer
  value <- getStack
  st <- get
  let vars = M.insert (fromIntegral n) value (vmVariables st)
  put $ st {vmVariables = vars}

readVar :: VM ()
readVar = do
  n <- getArg :: VM Integer
  vars <- gets vmVariables
  case M.lookup (fromIntegral n) vars of
    Nothing -> fail $ "Trying to read variable before assignment: #" ++ show n
    Just value -> pushS value

input :: VM ()
input = do
  str <- lift getLine
  if all isDigit str
    then pushS (SInteger $ read str)
    else pushS (SString str)

mark :: VM ()
mark = do
  pc <- gets vmPC
  pushS (SInteger $ fromIntegral pc)

branch :: Int -> VM ()
branch n = do
  st <- get
  put $ st {vmPC = n}

goto :: VM ()
goto = do
  n <- getArg :: VM Integer
  branch (fromIntegral n)

jumpIf :: (Integer -> Bool) -> VM ()
jumpIf test = do
  addr <- getArg :: VM Integer
  cond <- getStack
  case cond of
    SInteger i -> if test i
                    then branch (fromIntegral addr)
                    else step
    _ -> fail $ "Condition value is " ++ showType cond ++ ", not Integer!"

getMark :: Marks -> String -> VM ()
getMark marks name = do
  case M.lookup name marks of
    Just x -> pushS (SInteger $ fromIntegral x)
    Nothing -> fail $ "Undefined mark: " ++ name

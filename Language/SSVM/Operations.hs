{-# LANGUAGE ScopedTypeVariables #-}
module Language.SSVM.Operations
  (push, pushS, pushD,
   pop, dup, swap, over,
   printStack, printCurrentDef, printF,
   add, sub, neg, mul, divide, absF,
   remF, cmpF,
   variable, recall, assign, readVar, define,
   allocArray, readArray, assignArray,
   goto, jumpIf,
   mark, getMark,
   input,
   step
  ) where

import Data.Data
import Data.Char
import Data.Array
import qualified Data.Map as M
import Control.Monad.State

import Language.SSVM.Types

-- | Increment PC
step :: VM ()
step = do
  st <- get
  let was = vmPC st
  put $ st {vmPC = was + 1}

-- | Change stack with given function
withStack :: (Stack -> Stack) -> VM ()
withStack fn = do
  st <- get
  let stk = vmStack st
  setStack (fn stk)

-- | Change stack with given monadic action
withStackM :: (Stack -> VM Stack) -> VM ()
withStackM fn = do
  st <- get
  let stk = vmStack st
  stk' <- fn stk
  setStack stk'

-- | Set VM stack
setStack :: Stack -> VM ()
setStack stk = do
  st <- get
  put $ st { vmStack = stk }

-- | Push value to the stack
push :: (StackType a) => a -> VM ()
push x = withStack (toStack x:)

-- | Push stack item to the stack
pushS :: StackItem -> VM ()
pushS x = withStack (x:)

-- | Add item to current definition
pushD :: StackItem -> VM ()
pushD x = do
  st <- get
  let def = vmCurrentDefinition st
  put $ st {vmCurrentDefinition = (x:def)}

-- | Empty current definition
endDef :: VM ()
endDef = do
  st <- get
  put $ st {vmCurrentDefinition = []}

-- | Drop stack head
-- (a -- )
pop :: VM ()
pop = withStackM pop'
  where
    pop' [] = fail "DROP on empty stack!"
    pop' (x:xs) = return xs

-- | Duplicate stack head
-- (a -- a a)
dup :: VM ()
dup = withStackM dup'
  where
    dup' [] = fail "DUP on empty stack!"
    dup' (x:xs) = return (x:x:xs)

-- | Swap two top items on the stack
-- (a b -- b a)
swap :: VM ()
swap = withStackM swap'
  where
    swap' [] = fail "SWAP on empty stack!"
    swap' [_] = fail "SWAP on single-element stack!"
    swap' (x:y:xs) = return (y:x:xs)

-- | (a b -- a b a)
over :: VM ()
over = withStackM over'
  where
    over' [] = fail "OVER on empty stack!"
    over' [_] = fail "OVER on single-element stack!"
    over' (x:y:xs) = return (y:x:y:xs)

-- | Print stack content
printStack :: VM ()
printStack = do
  stk <- gets vmStack
  lift $ putStrLn $ unwords $ map showPrint stk

-- | Print current definition
printCurrentDef :: VM ()
printCurrentDef = do
  def <- gets vmCurrentDefinition
  lift $ putStr "Current definition: "
  lift $ putStrLn $ unwords $ map showItem (reverse def)

-- | Get stack head (and drop it from stack)
-- (a -- )
getStack :: VM StackItem
getStack = do
  stk <- gets vmStack
  case stk of
    [] -> fail "Trying to get element from empty stack!"
    (x:xs) -> do
              setStack xs
              return x

-- | Get stack head (and drop it from stack)
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

-- | Run given function on stack head
-- (a -- f(a))
liftF :: (StackType a) => (a -> a) -> VM ()
liftF fn = do
  x <- getArg
  push (fn x)

-- | Run given operation on two top stack items
-- (a b -- a `op` b)
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
      _ -> fail $ "Invalid types on CMP: " ++ showType x ++ ", " ++ showType y
  where
    cmp :: (Ord a) => a -> a -> Integer
    cmp a b = case compare a b of
               LT -> -1
               EQ -> 0
               GT -> 1

-- | Print stack head
-- (a -- )
printF :: VM ()
printF = do
  x <- getStack
  lift $ putStr $ showPrint x

-- | Define word
define :: VM ()
define = do
    ws <- gets vmCurrentDefinition
    endDef
    w <- getStack
    col <- getStack
    when (col /= SInstruction COLON) $
      fail $ "No COLON before DEFINE!"
    case w of
      SString name -> do
        st <- get
        dict <- gets vmDefinitions
        pc <- gets vmPC
        let start = pc - length ws
            dict' = M.insert name (Definition start $ reverse ws) dict
        put $ st {vmDefinitions = dict'}
      x -> fail $ "New word name is " ++ showType x ++ ", not String!"

-- | Recall word definition
recall :: String -> VM Definition
recall name = do
  dict <- gets vmDefinitions
  case M.lookup name dict of
    Nothing -> fail $ "Unknown word: " ++ name
    Just list -> return list

-- | Define variable
variable :: VM ()
variable = do
  name <- getArg
  col <- getStack
  when (col /= SInstruction COLON) $
    fail $ "No COLON before VARIABLE!"
  st <- get
  pc <- gets vmPC
  let n = vmNextVariable st
      dict = M.insert name (Definition (pc-1) [SInteger $ fromIntegral n]) (vmDefinitions st)
  put $ st {vmDefinitions = dict, vmNextVariable = n+1}

-- | Assign value to variable
-- (value variable-number -- )
assign :: VM ()
assign = do
  n <- getArg
  value <- getStack
  st <- get
  let vars = M.insert n value (vmVariables st)
  put $ st {vmVariables = vars}

-- | Read variable value
-- (variable-number -- value)
readVar :: VM ()
readVar = do
  n <- getArg
  vars <- gets vmVariables
  case M.lookup n vars of
    Nothing -> fail $ "Trying to read variable before assignment: #" ++ show n
    Just value -> pushS value

-- | Allocate an array
-- (size variable-number -- )
allocArray :: VM ()
allocArray = do
  a  <- getArg
  sz <- getArg :: VM Int
  st <- get
  let arr = listArray (1,sz) (replicate sz $ SInteger 0)
      vars = M.insert a (SArray arr) (vmVariables st)
  put $ st {vmVariables = vars}

-- | Assign value to array item.
-- (value array-variable-number index -- )
assignArray :: VM ()
assignArray = do
  i <- getArg
  a <- getArg
  value <- getStack
  st <- get
  let vars = vmVariables st
  case M.lookup a vars of
    Just (SArray arr) -> do
                  let vars' = M.insert a (SArray (arr // [(i, value)])) vars
                  put $ st {vmVariables = vars'}
    Just x -> fail $ "On [!]: variable type is " ++ showType x ++ ", not Array!"
    Nothing -> fail $ "Trying to assign array item before array allocation!"

-- | Read item from array.
-- (array-variable-number index -- value)
readArray :: VM ()
readArray = do
  i <- getArg
  a <- getArg
  st <- get
  let vars = vmVariables st
  case M.lookup a vars of
    Just (SArray arr) -> pushS (arr ! i)
    Just x -> fail $ "On [@]: variable type is " ++ showType x ++ ", not Array!"
    Nothing ->  fail "Trying to read array item before array allocation!"

-- | Read value from stdin
-- ( -- value)
input :: VM ()
input = do
  str <- lift getLine
  if all isDigit str
    then pushS (SInteger $ read str)
    else pushS (SString str)

-- | Mark at current PC
-- ( -- pc)
mark :: VM ()
mark = do
  pc <- gets vmPC
  pushS (SInteger $ fromIntegral pc)

-- | Go to named instruction
branch :: Int -> VM ()
branch n = do
  st <- get
  put $ st {vmPC = n}

-- | Get PC from stack
-- (pc -- )
goto :: VM ()
goto = do
  n <- getArg :: VM Integer
  branch (fromIntegral n)

-- | Jump to given address if condition is satisfied
jumpIf :: (Integer -> Bool) -> VM ()
jumpIf test = do
  addr <- getArg :: VM Integer
  cond <- getStack
  case cond of
    SInteger i -> if test i
                    then branch (fromIntegral addr)
                    else step
    _ -> fail $ "Condition value is " ++ showType cond ++ ", not Integer!"

-- | Get mark by name
-- ( -- pc)
getMark :: [Marks] -> String -> VM ()
getMark [] _ = fail $ "Internal error: getMark with empty marks stack!"
getMark (marks:_) name = do
  case M.lookup name marks of
    Just x -> pushS (SInteger $ fromIntegral x)
    Nothing -> fail $ "Undefined mark: " ++ name


{-# LANGUAGE ScopedTypeVariables #-}
module Language.SimpleForth.Operations where

import Data.Data
import qualified Data.Map as M
import Control.Monad.State

import Language.SimpleForth.Types

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
  x <- getArg
  y <- getArg
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
        let dict' = M.insert name ws dict
        put $ st {vmDefinitions = dict'}
      x -> fail $ "New word name is " ++ showType x ++ ", not String!"

recall :: String -> Forth [StackItem]
recall name = do
  dict <- gets vmDefinitions
  case M.lookup name dict of
    Nothing -> fail $ "Unknown word: " ++ name
    Just list -> return (reverse list)


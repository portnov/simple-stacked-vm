
module Language.SimpleForth.Interpreter where

import Control.Monad
import Control.Monad.State

import Language.SimpleForth.Types
import Language.SimpleForth.Operations

interpret :: Stack -> Forth ()
interpret list = mapM_ interpretOne list

printItem :: StackItem -> Forth()
printItem i = do
  lift $ putStr ">> "
  lift $ putStrLn $ showItem i

traceStack :: Stack -> Forth ()
traceStack list = mapM_ (\i -> printItem i >> interpretOne i >> printStack >> printCurrentDef) list

runForth :: Forth () -> IO ()
runForth forth = evalStateT forth emptyVMState

interpretOne :: StackItem -> Forth ()
interpretOne (SInteger x) = push x
interpretOne (SString x)  = push x
interpretOne (SInstruction x) = eval x
interpretOne (Quote x) = pushD x

eval :: Instruction -> Forth ()
eval NOP      = return ()
eval (PUSH x) = pushS x
eval DROP     = pop
eval DUP      = dup
eval SWAP     = swap
eval OVER     = over
eval PRINT    = printF
eval PRINTALL = printStack
eval ADD      = add
eval MUL      = mul
eval DIV      = divide
eval REM      = remF
eval SUB      = sub
eval NEG      = neg
eval ABS      = absF
eval DEFINE   = define
eval COLON    = push COLON
eval (CALL s) = interpret =<< recall s
eval VARIABLE = variable
eval ASSIGN   = assign
eval READ     = readVar


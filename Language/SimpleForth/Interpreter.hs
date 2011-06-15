
module Language.SimpleForth.Interpreter where

import Control.Monad
import Control.Monad.State

import Language.SimpleForth.Types
import Language.SimpleForth.Operations

interpret :: Stack -> Forth ()
interpret list = mapM_ interpretOne list

runForth :: Forth () -> IO ()
runForth forth = evalStateT forth emptyVMState

interpretOne :: StackItem -> Forth ()
interpretOne (SInteger x) = push x
interpretOne (SString x)  = push x
interpretOne (SInstruction x) = eval x

eval :: Instruction -> Forth ()
eval NOP      = return ()
eval (PUSH x) = pushS x
eval DROP     = pop
eval DUP      = dup
eval SWAP     = swap
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
eval CALL     = interpret =<< recall

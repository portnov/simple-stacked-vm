
module Language.SSVM.Interpreter
  (interpret,
   runVM,
   runVM',
   traceVM
  ) where

import Control.Monad.State
import qualified Data.Map as M

import Language.SSVM.Types
import Language.SSVM.Operations

-- | Interpret code
interpret :: Code -> VM ()
interpret c@(Code marks code) = do
  t <- gets vmTraceMode
  if t
    then traceStack c
    else interpretWith (interpretOne marks) code

interpretWith :: (StackItem -> VM ()) -> Stack -> VM ()
interpretWith go code = do
  pc <- gets vmPC
  if pc >= length code
    then return ()
    else do
         go (code !! pc)
         interpretWith go code

printItem :: StackItem -> VM ()
printItem i = do
  pc <- gets vmPC
  lift $ putStr $ show pc
  lift $ putStr ".>>\t"
  lift $ putStrLn $ showItem i

traceStack :: Code -> VM ()
traceStack (Code marks code) = do
    lift $ putStrLn $ "Trace marks: " ++ show marks
    lift $ putStrLn $ "Trace code: " ++ show code
    lift $ putStr "Trace stack: "
    printStack
    interpretWith traceOne code
  where
    traceOne i = do
      printItem i
      interpretOne marks i
      printStack
      printCurrentDef

-- | Run VM
runVM :: VM () -> IO ()
runVM forth = evalStateT forth emptyVMState

runVM' :: VMState -> VM () -> IO ()
runVM' st forth = evalStateT forth st

-- | Run VM in trace mode
traceVM :: VM () -> IO ()
traceVM code = runVM' (emptyVMState {vmTraceMode = True}) code

interpretOne :: [Marks] -> StackItem -> VM ()
interpretOne _ (SInteger x) = push x >> step
interpretOne _ (SString x)  = push x >> step
interpretOne m (SInstruction x) = eval m x
interpretOne _ (SArray _) = fail "Array literals are not supported"
interpretOne _ (Quote x) = pushD x >> step

interpretLocal :: Int -> Code -> VM ()
interpretLocal pc code = do
  let oldMarks = cMarks code
      newMarks = shiftMarks pc (last oldMarks)
      code' = code {cMarks = newMarks:oldMarks}
  st <- get
  let oldPC = vmPC st
  put $ st {vmPC = 0}
  interpret code'
  st <- get
  put $ st {vmPC = oldPC}

shiftMarks :: Int -> Marks -> Marks
shiftMarks k = M.map shift
  where
    shift n = n-k

-- | Evaluate one instruction
eval :: [Marks] -> Instruction -> VM ()
eval _ NOP      = step
eval _ (PUSH x) = pushS x >> step
eval _ DROP     = pop >> step
eval _ DUP      = dup >> step
eval _ SWAP     = swap >> step
eval _ OVER     = over >> step
eval _ PRINT    = printF >> step
eval _ PRINTALL = printStack >> step
eval _ ADD      = add >> step
eval _ MUL      = mul >> step
eval _ DIV      = divide >> step
eval _ REM      = remF >> step
eval _ SUB      = sub >> step
eval _ NEG      = neg >> step
eval _ ABS      = absF >> step
eval _ CMP      = cmpF >> step
eval _ DEFINE   = define >> step
eval _ COLON    = push COLON >> step
eval m (CALL s) = do
                  (Definition pc code) <- recall s
--                   lift $ putStrLn $ "Calling to " ++ show pc
                  interpretLocal pc $ Code m code
                  step
eval _ VARIABLE = variable >> step
eval _ ASSIGN   = assign >> step
eval _ READ     = readVar >> step
eval _ INPUT    = input >> step
eval _ MARK     = mark >> step
eval m (GETMARK x) = getMark m x >> step
eval _ GOTO     = goto
eval _ JZ       = jumpIf (== 0)
eval _ JNZ      = jumpIf (/= 0)
eval _ JGT      = jumpIf (> 0)
eval _ JLT      = jumpIf (< 0)
eval _ JGE      = jumpIf (>= 0)
eval _ JLE      = jumpIf (<= 0)


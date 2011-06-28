
import Control.Monad (when)
import qualified Data.Map as M
import System.Environment
import System.Console.GetOpt
import System.FilePath
import System.IO

import Language.SSVM.Types
import Language.SSVM.Parser
import Language.SSVM.Interpreter
import Language.SSVM.Binary

data Flag =
    Mode Mode
  | Output FilePath
  deriving (Eq, Show)

data Mode =
    Interpret
  | Trace
  | TraceBytecode
  | Compile
  | Run
  | Decompile
  | Help
  deriving (Eq, Show)

data RunMode = RunMode {
  mode :: Mode,
  inputFile :: FilePath,
  outputFile :: Maybe FilePath }
  deriving (Eq, Show)

defaultMode = RunMode {
  mode = Run,
  inputFile = "-",
  outputFile = Nothing }

options :: [OptDescr Flag]
options = [
  Option "c" ["compile"]   (NoArg $ Mode Compile)   "compile source code to bytecode",
  Option "i" ["interpret"] (NoArg $ Mode Interpret) "interpret source code",
  Option "e" ["run"]       (NoArg $ Mode Run)       "run compiled bytecode",
  Option "d" ["decompile"] (NoArg $ Mode Decompile) "decompile bytecode into pseudo source code",
  Option "t" ["trace"]     (NoArg $ Mode Trace)     "interpret and trace source code ",
  Option ""  ["trace-bytecode"] (NoArg $ Mode TraceBytecode)     "run and trace bytecode ",
  Option "o" ["output"]    (ReqArg Output "FILE")   "set output file name",
  Option "h" ["help"]      (NoArg $ Mode Help)      "show this help and exit" ]

usage :: String
usage = usageInfo header options
  where
    header = "Usage: ssvm [MODE] [-o OUTPUT] FILE"

flags2runmode :: [Flag] -> RunMode
flags2runmode flags = foldl go defaultMode flags
  where
    go acc (Mode m)      = acc {mode = m}
    go acc (Output path) = acc {outputFile = Just path}

parseCmdLine :: [String] -> Either String RunMode
parseCmdLine args =
  case getOpt Permute options args of
    (flags, [], []) | Mode Help `elem` flags -> Right (flags2runmode flags)
                    | otherwise              -> Left "No input file"
    (flags, [file], []) -> Right $ (flags2runmode flags) {inputFile = file}
    (_, (_:_:_), [])    -> Left "More than one input file"
    (_, _, errs)        -> Left $ unlines errs ++ usage

doInterpret :: FilePath -> IO ()
doInterpret path = do
  mbCode <- parseSourceFile path
  case mbCode of
    Left err -> fail (show err)
    Right code -> runVM (interpret code)

doTrace :: FilePath -> IO ()
doTrace path = do
  mbCode <- parseSourceFile path
  case mbCode of
    Left err -> fail (show err)
    Right code -> traceVM (interpret code)

doTraceBytecode :: FilePath -> IO ()
doTraceBytecode path = do
  code <- loadCode path
  traceVM (interpret code)

doCompile :: FilePath -> Maybe FilePath -> IO ()
doCompile src mbdst = do
  let dst = case mbdst of
              Just x -> x
              Nothing -> replaceExtension src ".bytecode"
  mbCode <- parseSourceFile src
  case mbCode of
    Left err -> fail (show err)
    Right code -> dumpCode dst code

doRun :: FilePath -> IO ()
doRun path = do
  code <- loadCode path
  runVM (interpret code)

doDecompile :: FilePath -> IO ()
doDecompile path = do
    Code marks code <- loadCode path
    putStrLn $ unwords $ zipWith (showOne $ head marks) [1..] code
  where
    showOne ms n item =
      if n `elem` M.elems ms
        then showItem item ++ " .mark_at_" ++ show n
        else showItem item

main = do
  args <- getArgs
  case parseCmdLine args of
    Left err -> error err
    Right m -> do
      term <- hIsTerminalDevice stdout
      when term $
        hSetBuffering stdout NoBuffering
      case mode m of
        Interpret -> doInterpret (inputFile m)
        Compile   -> doCompile (inputFile m) (outputFile m)
        Run       -> doRun (inputFile m)
        Decompile -> doDecompile (inputFile m) 
        Trace     -> doTrace (inputFile m) 
        TraceBytecode -> doTraceBytecode (inputFile m) 
        Help      -> putStrLn usage


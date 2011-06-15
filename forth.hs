
import System.Environment
import System.Console.GetOpt
import System.FilePath
import Data.Binary

import Language.SimpleForth.Types
import Language.SimpleForth.Parser
import Language.SimpleForth.Interpreter
import Language.SimpleForth.Binary

data Flag =
    Mode Mode
  | Output FilePath
  deriving (Eq, Show)

data Mode =
    Interpret
  | Compile
  | Run
  | Decompile
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
  Option "d" ["decompile"] (NoArg $ Mode Decompile) "decompile bytecode into source code",
  Option "o" ["output"]    (ReqArg Output "FILE")   "set output file name" ]

usage :: String
usage = usageInfo header options
  where
    header = "Usage: forth [-e|-c|-i] [-o OUTPUT] FILE"

flags2runmode :: [Flag] -> RunMode
flags2runmode flags = foldl go defaultMode flags
  where
    go acc (Mode m)      = acc {mode = m}
    go acc (Output path) = acc {outputFile = Just path}

parseCmdLine :: [String] -> Either String RunMode
parseCmdLine args =
  case getOpt Permute options args of
    (_, [], [])         -> Left "No input file"
    (flags, [file], []) -> Right $ (flags2runmode flags) {inputFile = file}
    (_, (_:_:_), [])    -> Left "Too many input files"
    (_, _, errs)        -> Left $ unlines errs ++ usage

doInterpret :: FilePath -> IO ()
doInterpret path = do
  mbCode <- parseForthFile path
  case mbCode of
    Left err -> fail (show err)
    Right code -> runForth (interpret code)

doCompile :: FilePath -> Maybe FilePath -> IO ()
doCompile src mbdst = do
  let dst = case mbdst of
              Just x -> x
              Nothing -> replaceExtension src ".bytecode"
  mbCode <- parseForthFile src
  case mbCode of
    Left err -> fail (show err)
    Right code -> encodeFile dst code

doRun :: FilePath -> IO ()
doRun path = do
  code <- decodeFile path
  runForth (interpret code)

doDecompile :: FilePath -> IO ()
doDecompile path = do
  code <- decodeFile path
  putStrLn $ unwords $ map showItem code

main = do
  args <- getArgs
  case parseCmdLine args of
    Left err -> error err
    Right m -> do
      case mode m of
        Interpret -> doInterpret (inputFile m)
        Compile   -> doCompile (inputFile m) (outputFile m)
        Run       -> doRun (inputFile m)
        Decompile -> doDecompile (inputFile m) 


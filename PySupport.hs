module PySupport where

import Control.Exception (onException)
import Control.Monad (void)
import Data.Char (chr, ord)
import Data.List (stripPrefix)
import System.Environment (lookupEnv)
import System.Info (os)
import System.IO
import System.Process
import System.Timeout (timeout)
import Text.Read (readMaybe)

newtype PythonCode = PCode { text :: String }
type PythonHandle = (Handle, Handle, ProcessHandle)

-- Use an explicit Python path when one has been set.
pythonCommand :: IO (FilePath, [String])
pythonCommand = do
  path <- lookupEnv "LAMBEK_PYTHON"
  return $ case path of
    Just p | not (null p) -> (p, [])
    _ | os == "mingw32" -> ("py", ["-3"])
      | otherwise -> ("python3", [])

-- Keep the world in one Python process. Character codes let us send code and results containing quotes, newlines or Unicode on one line.
worker :: String
worker = unlines
  [ "import ast, contextlib, io, sys"
  , "scope = {'__name__': '__main__'}"
  , "for line in sys.stdin:"
  , "    output = io.StringIO()"
  , "    try:"
  , "        code = ''.join(map(chr, ast.literal_eval(line)))"
  , "        with contextlib.redirect_stdout(output), contextlib.redirect_stderr(output):"
  , "            exec(code, scope, scope)"
  , "        reply = 'OK ' + output.getvalue()"
  , "    except BaseException as error:"
  , "        reply = 'ERR ' + type(error).__name__ + ': ' + str(error)"
  , "    print([ord(c) for c in reply], flush=True)"
  ]

launchPython :: PythonCode -> IO PythonHandle
launchPython world = do
  (exe, args) <- pythonCommand
  (Just input, Just output, _, process) <-
    createProcess (proc exe (args ++ ["-u", "-c", worker]))
      { std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit }
  let python = (input, output, process)
  (do hSetBuffering input LineBuffering
      void (runPythonCode python world)
      return python) `onException` closePython python

runPythonCode :: PythonHandle -> PythonCode -> IO String
runPythonCode python@(input, output, _) code = do
  response <- timeout 5000000 $ do
    hPutStrLn input (show (map ord (text code)))
    hFlush input
    hGetLine output
  case response of
    Nothing -> do
      closePython python
      ioError (userError "Python timed out; restart the chat.")
    Just line -> case readMaybe line :: Maybe [Int] of
      Just codes | all (\n -> n >= 0 && n <= 0x10ffff) codes ->
        let reply = map chr codes in
        case stripPrefix "OK " reply of
          Just value -> return (trimNewline value)
          Nothing -> ioError (userError reply)
      _ -> ioError (userError "Invalid reply from Python")
  where
    trimNewline value = case reverse value of
      '\n':rest -> reverse rest
      _ -> value

closePython :: PythonHandle -> IO ()
closePython (input, output, process) =
  cleanupProcess (Just input, Just output, Nothing, process)
import Data.Char
import System.IO
import Control.Exception

import LTypes
import LExp
import qualified Lambek

import Parser
import PySupport

-- "standardize" the capitalization of a word by making the first
-- letter uppercase and the following letters lowercase.
-- (This will make it easier to look up words in the lexicon.)
stdize (c:cs) = toUpper c : map toLower cs

-- read-eval-print-loop
repl :: Lexicon PythonCode -> PythonHandle -> IO ()
repl elx phandle = do
  -- print the prompt
  putStr "> "
  done <- isEOF
  if done then return () else do
    -- get a line of input
    line <- getLine
    if line == ":quit" then return () else do
      -- split it up into "standardized" words
      let ws = map stdize $ words $ map (\c -> if c == '?' then ' ' else c) line
      -- compute all derivations with their lexical meanings
      let ds = Lambek.readings elx ws
            ++ Lambek.readings (elx { stp = Atm "q" }) ws
      -- no derivation, sentence is not grammatical.
      -- >1 derivation means it is ambiguous
      case ds of
        []  -> putStrLn "That is not a well-formed sentence"
        [d] -> showReading False d
        _   -> do
          putStrLn "That sentence is ambiguous"
          mapM_ (showReading True) ds
      -- repeat
      repl elx phandle
  where
    -- evaluate a reading in Python and print its value
    showReading ambiguous (term, meanings) = do
      if ambiguous then putStr ("Sense [" ++ prettyLExp term ++ "]: ") else return ()
      case Lambek.toPython meanings term of
        Left err -> putStrLn ("Evaluation error: " ++ err)
        Right code -> do
          result <- try (runPythonCode phandle (PCode ("try:\n  print(" ++ text code ++ ")\nexcept Exception as e:\n  print('Evaluation error: ' + str(e))\n")))
                    :: IO (Either IOError String)
          case result of
            Left err -> putStrLn ("Evaluation error: " ++ show err)
            Right value -> putStrLn value

main = do
  -- send console output immediately to the user
  hSetBuffering stdout NoBuffering
  -- input a lexicon file
  elx <- getLex
  -- input a database of facts
  db <- getDB
  -- launch a Python interpreter
  phandle <- launchPython db
  -- start the read-eval-print loop
  putStrLn "Starting chat..."
  repl elx phandle
  where
    getDB :: IO PythonCode
    getDB = do
      putStr "Facts about the world: "
      dname <- getLine
      mdb <- try (readFile dname) :: IO (Either IOError String)
      case mdb of
        Left err -> putStrLn ("Error loading file " ++ show err) >> getDB
        Right db -> return (PCode db)

    getLex :: IO (Lexicon PythonCode)
    getLex = do
      putStr "Lexicon file: "
      lname <- getLine
      res <- parseFile lexicon lname
      case res of
        Left err -> putStrLn ("Error loading file " ++ show err) >> getLex
        Right (Left err) -> putStrLn ("parse error at " ++ lname ++ ":" ++ show err) >> getLex
        Right (Right lex) -> return lex
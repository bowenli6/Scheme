module Main where
import Scheme.Runtime
import Scheme.Core
import Scheme.Parse
import Scheme.Eval

import Prelude hiding (lookup)
import System.IO (hFlush, hPutStr, hPutStrLn, hGetLine, stdin, stdout)
import Text.ParserCombinators.Parsec hiding (Parser, State)
import Control.Monad
import Control.Monad.State
import Control.Monad.Except

--- ### REPL

printLn :: String -> IO ()
printLn str = hPutStrLn stdout str >> hFlush stdout

repl :: Env -> IO ()
repl env = do
  putStr "scheme> "
  hFlush stdout
  l <- getLine                                        -- Read
  case parse exprP "Expression" l of                  -- Parse
    Left err -> print err                             -- Diagnostics
    Right expr ->
        -- runExcept returns a value of type `Either Diagnostic (Val, Env)`
      case runExcept $ runStateT (eval expr) env of   -- Eval
        Left err -> print err
        Right (val, env') -> 
          case val of 
            Void -> repl env'
            _    -> do print val
                       repl env'
main :: IO ()
main = repl runtime

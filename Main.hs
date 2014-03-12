import Data.Attoparsec.Text

import Commands
import Data.Text
import Executer
import Parser
import System.IO
import System.IO.Unsafe
main = do
  --listen to commands
  hFlush stdout
  input <- readCommand
  interpret input
  
  where
    interpret :: String -> IO ()
    interpret input = do
      let command' = parseOnly commandParser (pack input)
      case command' of
        (Right command) -> do
          putStrLn $ "Executing ..."
          result <- execute command
          putStrLn $ show result          
        (Left err) -> do
          putStrLn $ err
          putStrLn $ "Could not understand your input"
      -- do it again, for the sake of IO
      readCommand >>= interpret

    readCommand :: IO String
    readCommand = do
      hFlush stdout
      putStr "azhen> " >> hFlush stdout
      input <- getLine
      return input
      
{-
   interact consoleInterpreter
  where
    consoleInterpreter :: String -> String
    consoleInterpreter input = do      
      --let input = pack $ "CREATE DATABASE nombreDeMiDB"     
      let _ = unsafePerformIO $ putStrLn $ "Read " ++ input
      let cmd = parseOnly commandParser (pack input)

      case cmd of
        (Right command) -> let result = unsafePerformIO $ execute command
                           in show result             
        (Left s) -> error $ "Parsing " ++ s
     
-}

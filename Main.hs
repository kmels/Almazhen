import Data.Attoparsec.Text

import Commands
import Data.Text
import Executer
import Parser

main = do     
     let input = pack $ "CREATE DATABASE nombreDeMiDB"
     
     let cmd = parseOnly commandParser input

     case cmd of
       (Right command) -> execute command
       (Left s) -> error $ "Parsing " ++ s
     

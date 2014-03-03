import Data.Attoparsec.Text

import Commands
import Parser
import Data.Text

main = do     
     let input = pack $ "CREATE DATABASE nombreDeMiDB"
     
     let cmd = parseOnly commandParser input
     
     print cmd


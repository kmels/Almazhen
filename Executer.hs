{-# LANGUAGE OverloadedStrings #-}

module Executer where

import Commands
import Data.Text
import Database

-- | 
-- Executive funs
data Result = OK | Error String deriving Show

execute :: Command -> IO Result
execute (CREATEDB dbname) = createDB . unpack $ dbname

-- | Creates a database file
createDB :: String -> IO Result
createDB db_name = do
	 -- make an empty database
	 let new_db = Database { name = db_name, num_reg = 0 }
	 -- update the list of databases 
	 dbs <- getDatabases
         writeDatabases (new_db:dbs)
         return $ OK
	 

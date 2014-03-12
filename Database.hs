{-# LANGUAGE OverloadedStrings #-}

module Database where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as Char8
import           System.Directory(doesFileExist)
import           Data.Aeson

data Database = Database { name :: String, num_reg :: Int} deriving Show

-- |
-- DB Structure and structure-mappings from and to Json
instance FromJSON Database where
	 parseJSON (Object o) = Database <$> 
	 	   	      	       o .: "name" <*>
				       o .: "n_registers"

	 -- if no object could be parsed, 
	 parseJSON _ 	      = mzero

instance ToJSON Database where
   toJSON (Database name num_reg) = object ["name" .= name, "n_registers" .= num_reg]
   
-- |
-- Filesystem funs
-- 
databases_file_name = "C:/Almazhen/databases.meta"

-- | Reads the file of databases and returns a list of them.
getDatabases :: IO [Database]
getDatabases = do
  -- if file does not exist, return an empty list
  fileExists <- doesFileExist databases_file_name
  case fileExists of
    False -> return [] -- file does not exist, return an empty list
    True -> do
      -- read the file
      dbs_file_contents <- readFile databases_file_name >>= return . Char8.pack
      let dbs = decode dbs_file_contents
      case dbs of
        Just database_list -> return database_list
        Nothing -> error "The impossible happened at getDatabases. Could not parse list of databases from databases.meta"

writeDatabases :: [Database] -> IO ()
writeDatabases dbs = do
  let json = encode $ dbs
  Char8.putStrLn json
  putStrLn databases_file_name
  BL.writeFile databases_file_name json
  

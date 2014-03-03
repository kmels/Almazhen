module Parser where

import Commands
import Data.Text
import Data.Char(isSpace)
import qualified Data.Attoparsec.Text as Atto
import Control.Applicative((<|>))

commandParser :: Atto.Parser Command
commandParser = 
	      parseCmdCreateDB
--	      <|> parseCmdUseDB

-- | Parses the command to create a database
parseCmdCreateDB :: Atto.Parser Command
parseCmdCreateDB = do
		 cmd <- Atto.asciiCI $ pack "CREATE DATABASE"
		 _ <- Atto.space
		 dbName <- takeWord
		 return $ CREATEDB dbName 

-- | Parsers the command to use a ddatabase
-- parseCmdUseDB :: Atto.Parser Text
-- parseCmdUseDB = Atto.asciiCI $ "TODO"

takeWord :: Atto.Parser Text
takeWord = Atto.takeWhile $ not . isSpace


module Commands where

import Data.Text

data TODO = TODO deriving Show
data AZType = ZHEN_Int | ZHEN_Float deriving Show
type ColumnSpec = (String, AZType)
type Restriction = TODO
type Predicate = TODO

data RestrictionName = PKName Text | FKName Text | CHECKName Text deriving Show
data OrderBy = NoOrder | ASCOrder | DESCOrder deriving Show

data Command = CREATEDB Text
     | ALTERDB Text Text
     | DROPDB Text
     | SHOWDB Text
     | USEDB Text
     | CREATETB Text [ColumnSpec] [Restriction]
     | RENAMETB Text Text
     | ADDCOLUMN Text ColumnSpec [Restriction]
     | ADDCONSTRAINT Text Restriction
     | DROPCOLUMN Text Text
     | DROPCONSTRAINT Text RestrictionName
     | DROPTB Text
     | SHOWTB
     | SHOWCOLUMN Text
     | INSERT Text [Text]
     | INSERTWITHCOLS Text [Text] [Text]
     | UPDATE String Predicate
     | DELETE Text [(Text,Text)] Predicate
     | SELECT [Text] Text Predicate OrderBy
     | SELECTALL Text Predicate OrderBy deriving Show
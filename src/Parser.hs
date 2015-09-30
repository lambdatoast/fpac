module Parser where

import Text.Parsec as P
import Control.Applicative 

type Key = String
type VarName = String
data PropAccessPath = PropAccessPath  VarName [Key] deriving (Show)

propAccessParser :: Parsec String st PropAccessPath
propAccessParser = PropAccessPath <$> (varName <* char '.') <*> accesses
  where
    varName = many1 $ noneOf ['.']
    accesses = (P.many $ noneOf ['.']) `sepBy` (char '.')

test p = parse p ""

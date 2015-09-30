module Parser where

import Text.Parsec as P
import Control.Applicative 

type PropName = String
type VarName = String
data PropAccessPath = PropAccessPath  VarName [PropName] deriving (Show)

propAccessParser :: Parsec String st PropAccessPath
propAccessParser = PropAccessPath <$> (varName <* char '.') <*> accesses
  where
    varName = many1 $ noneOf ['.']
    accesses = (P.many1 $ noneOf ['.']) `sepBy` (char '.')

test p = parse p ""

module Parser where

import Text.Parsec as P
import Control.Applicative ((<$>),(<*), (*>), (<*>)) 

type PropName = String
type VarName = String
data PropAccessPath = PropAccessPath  VarName [PropName] deriving (Show)

propAccessParser :: Parsec String st PropAccessPath
propAccessParser = PropAccessPath <$> (varName <* char '.') <*> accesses
  where
    identifier = alphaNum <|> char '_'
    varName = many1 identifier
    accesses = (P.many1 identifier) `sepBy` (char '.')

fileParser :: Parsec String st [PropAccessPath]
fileParser = P.many (P.many html *> propAccessParser <* P.many html) <* eof
  where
    html = spaces *> char '<' *> (many1 $ noneOf "<>") <* char '>' <* spaces

test p = parse p ""

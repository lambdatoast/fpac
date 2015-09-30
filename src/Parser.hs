module Parser (propAccessParser, fileParser, test) where

import Text.Parsec as P
import Control.Applicative ((<$>),(<*), (*>), (<*>)) 

type PropName = String
type VarName = String
data PropAccessPath = PropAccessPath  VarName [PropName] deriving (Show)

type Parser = Parsec String ()

propAccessParser :: Parser PropAccessPath
propAccessParser = PropAccessPath <$> (varName <* char '.') <*> accesses
  where
    identifier = alphaNum <|> char '_'
    varName = many1 identifier
    accesses = (P.many1 identifier) `sepBy` (char '.')

htmlTokens :: Parser Char
htmlTokens = oneOf "\"'.=<>/-" <|> alphaNum <|> space

garbageR = skipMany htmlTokens <* spaces
garbageL = spaces *> garbageR

htmlo = spaces *> char '<' *> skipMany alphaNum <* spaces
htmlc = skipMany (oneOf "> ") <* spaces

vanillaTag = htmlo *> htmlc

angularExpr :: Parser PropAccessPath
angularExpr = 
  (htmlo *> (string "ng-model=\"" *> propAccessParser <* string "\"") <* htmlc)
  <|> garbageL *> (string "{{" *> propAccessParser <* string "}}") <* garbageR

fileParser :: Parser [PropAccessPath]
fileParser = optional vanillaTag *> P.many angularExpr <* eof

test :: Parsec String () a -> String -> Either ParseError a
test p = parse p ""

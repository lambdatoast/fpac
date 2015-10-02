module Template.Parser (templateParser) where

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

ngAttr :: Parser a -> Parser a
ngAttr p = 
  spaces *>
  (string "ng-" *> P.many1 (alphaNum <|> oneOf "-") *> char '=' *> string "\"") 
  *> p <* string "\"" <* spaces

angularExpr :: Parser PropAccessPath
angularExpr = 
  garbageL *> (string "{{" *> propAccessParser <* string "}}") <* garbageR

templateParser :: Parser [PropAccessPath]
templateParser = optional vanillaTag *> ((++) <$> manyAttrs <*> manyExprs) <* eof
  where manyAttrs = (htmlo *> P.many (ngAttr propAccessParser) <* htmlc)
        manyExprs = P.many angularExpr

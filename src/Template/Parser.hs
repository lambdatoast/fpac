module Template.Parser (templateParser, PropAccessPath (..)) where

import Text.Parsec as P

type PropName = String
type VarName = String
data PropAccessPath = PropAccessPath  VarName [PropName] deriving (Show)

type Parser = Parsec String ()

propAccessParser :: Parser PropAccessPath
propAccessParser = PropAccessPath <$> (identifier <* char '.') <*> accesses
  where
    identifier = P.many1 (alphaNum <|> char '_')
    bracketAccess = char '[' *> identifier <* char ']'
    accesses = (try (identifier <* (P.many1 bracketAccess)) <|> identifier) `sepBy` (char '.')

htmlTokens :: Parser Char
htmlTokens = oneOf "\"'.=<>/-" <|> alphaNum <|> space

garbageR :: Parser ()
garbageR = skipMany htmlTokens <* spaces

garbageL :: Parser ()
garbageL = spaces *> garbageR

htmlo :: Parser ()
htmlo = spaces *> char '<' *> skipMany alphaNum <* spaces

htmlc :: Parser ()
htmlc = skipMany (oneOf "> ") <* spaces

ngAttr :: Parser a -> Parser a
ngAttr p = 
  spaces *>
  (string "ng-" *> P.many1 (alphaNum <|> oneOf "-") *> char '=' *> string "\"") 
  *> p <* string "\"" <* spaces

angularExpr :: Parser PropAccessPath
angularExpr = 
  garbageL *> (string "{{" *> propAccessParser <* string "}}") <* garbageR

templateParser :: Parser [PropAccessPath]
templateParser = ((++) <$> manyAttrs <*> manyExprs) <* eof
  where manyAttrs = (htmlo *> P.many (ngAttr propAccessParser) <* htmlc)
        manyExprs = P.many angularExpr

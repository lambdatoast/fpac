module Schema.Parser (schemaParser, Schema (..), Prop (..), Val (..)) where

import qualified Data.List as L
import Text.Parsec as P
import Schema.AST

type Parser = Parsec String ()

key :: Parser String
key = spaces *> P.many1 alphaNum <* spaces

kvSep :: Parser Char
kvSep = spaces *> char ':' <* spaces

propSep :: Parser Char
propSep = spaces *> char ',' <* spaces

simpleVal :: Parser Val
simpleVal = SVal <$> (concat . L.intersperse "." <$> ((P.many1 alphaNum) `sepBy` char '.'))

objVal :: Parser Val
objVal = OVal <$> between (char '{') (char '}') manyProps

arrVal :: Parser Val
arrVal = AVal <$> between (char '[') (char ']') val

val :: Parser Val
val = try objVal <|> arrVal <|> simpleVal

prop :: Parser Prop
prop = Prop <$> (key <* kvSep) <*> val

manyProps :: Parser [Prop]
manyProps = (between spaces spaces prop) `sepBy` propSep

schemaParser :: Parser Schema
schemaParser = Schema <$>
  (P.manyTill anyChar (try $ string "new Schema") *>
  open *> content <* close)
  where content = between (char '{') (char '}') manyProps
        open = char '('
        close = string ");" <* spaces <* eof

module Schema.Parser (schemaParser) where

import qualified Data.List as L
import Text.Parsec as P
import Control.Applicative ((<$>),(<*), (*>), (<*>), pure) 

type Parser = Parsec String ()
data Val = SVal String | OVal [Prop] deriving (Show)
data Prop = Simple String Val deriving (Show)
data Schema = Schema [Prop] deriving (Show)

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

prop :: Parser Prop
prop = Simple <$> (key <* kvSep) <*> (try objVal <|> simpleVal)

manyProps :: Parser [Prop]
manyProps = (between spaces spaces prop) `sepBy` propSep

schemaParser :: Parser Schema
schemaParser = Schema <$>
  (P.manyTill anyChar (try $ string "new Schema") *>
  open *> content <* close)
  where content = between (char '{') (char '}') manyProps
        open = char '('
        close = string ");" <* spaces <* eof

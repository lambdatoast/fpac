module SchemaParser (schemaParser) where

import Text.Parsec as P
import Control.Applicative ((<$>),(<*), (*>), (<*>), pure) 

type Parser = Parsec String ()
data Prop = Simple String String | Complex String Prop
data Doc = Doc [Prop] 

schemaParser :: Parser String
schemaParser = 
  P.manyTill anyChar (try $ string "new Schema") *>
  open *> content <* close
  where content = P.many1 $ noneOf ");"
        open = string "("
        close = string ");" <* spaces <* eof

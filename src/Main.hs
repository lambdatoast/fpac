module Main where

import qualified Text.Parsec as P
import qualified Template.Parser as TP
import qualified Schema.Parser as SP
import qualified Schema.Check as S

test :: P.Parsec String () a -> String -> Either P.ParseError a
test p = P.parse p ""

main :: IO ()
main = do
  source <- readFile "./test-data/simple.html"
  putStrLn $ show $ test TP.templateParser source
  source <- readFile "./test-data/blog.schema.js"
  putStrLn $ show $ test SP.schemaParser source
  putStrLn $ show $ (fmap (S.check ["date", "type"]) $ test SP.schemaParser source)

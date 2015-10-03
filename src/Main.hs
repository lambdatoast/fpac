module Main where

import qualified Data.List as L
import qualified Text.Parsec as P
import qualified Template.Parser as TP
import qualified Schema.Parser as SP
import qualified Schema.Check as S

test :: P.Parsec String () a -> String -> Either P.ParseError a
test p = P.parse p ""

checkPropAccess :: SP.Schema -> TP.PropAccessPath -> (String, Bool)
checkPropAccess schema (TP.PropAccessPath var xs) = (L.intercalate "." $ [var] ++ xs, S.check xs schema)

main :: IO ()
main = do
  templateSrc <- readFile "./test-data/simple.html"
  putStrLn $ show $ test TP.templateParser templateSrc
  schemaSrc <- readFile "./test-data/blog.schema.js"
  putStrLn $ show $ test SP.schemaParser schemaSrc
  putStrLn $ show $ do 
    propAccesses <- test TP.templateParser templateSrc
    schema <- test SP.schemaParser schemaSrc
    return $ map (checkPropAccess schema) propAccesses


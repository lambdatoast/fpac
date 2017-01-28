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

loadTemplate :: IO String
loadTemplate = readFile "./test-data/blog.html"

loadSchema :: IO String
loadSchema = readFile "./test-data/blog.schema.js"

printTemplate :: IO ()
printTemplate = do
  templateSrc <- loadTemplate
  putStrLn $ show $ test TP.templateParser templateSrc

printSchema :: IO ()
printSchema = do
  schemaSrc <- loadSchema
  putStrLn $ show $ test SP.schemaParser schemaSrc

printTemplateCheck :: IO ()
printTemplateCheck =
  do 
    templateSrc <- loadTemplate
    schemaSrc <- loadSchema
    putStrLn $ show $ do
      propAccesses <- test TP.templateParser templateSrc
      schema <- test SP.schemaParser schemaSrc
      return $ map (checkPropAccess schema) propAccesses

main :: IO ()
main = do
  printTemplate
  printSchema
  printTemplateCheck


module Main where

import qualified Data.List as L
import qualified Text.Parsec as P
import qualified Template.Parser as TP
import qualified Schema.Parser as SP
import qualified Schema.Check as S
import qualified Options as O

test :: P.Parsec String () a -> String -> Either P.ParseError a
test p = P.parse p ""

checkPropAccess :: SP.Schema -> TP.PropAccessPath -> (String, Bool)
checkPropAccess schema (TP.PropAccessPath var xs) = (L.intercalate "." $ [var] ++ xs, S.check xs schema)

loadTemplate :: String -> IO String
loadTemplate = readFile

loadSchema :: String -> IO String
loadSchema = readFile

printTemplate :: String -> IO ()
printTemplate filePath = do
  templateSrc <- loadTemplate filePath
  putStrLn $ show $ test TP.templateParser templateSrc

printSchema :: String -> IO ()
printSchema filePath = do
  schemaSrc <- loadSchema filePath
  putStrLn $ show $ test SP.schemaParser schemaSrc

printTemplateCheck :: String -> String -> IO ()
printTemplateCheck tplFilePath schFilePath =
  do 
    templateSrc <- loadTemplate tplFilePath
    schemaSrc <- loadSchema schFilePath
    putStrLn $ show $ do
      propAccesses <- test TP.templateParser templateSrc
      schema <- test SP.schemaParser schemaSrc
      return $ map (checkPropAccess schema) propAccesses

data MainOptions = MainOptions 
    { optTemplate :: String
    , optSchema :: String
    }

instance O.Options MainOptions where
  defineOptions = pure MainOptions
    <*> O.simpleOption "template" "./test-data/blog.html" "Path to the view template"
    <*> O.simpleOption "schema" "./test-data/blog.schema.js" "Path to the db schema"

main :: IO ()
main = O.runCommand $ \opts args -> do
  let (template, schema) = (optTemplate opts, optSchema opts)
  printTemplate template
  printSchema schema
  printTemplateCheck template schema


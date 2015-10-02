module Main where

import qualified Parser as P
import qualified Schema.Parser as SP
import qualified Schema.Check as S

main :: IO ()
main = do
  source <- readFile "./test-data/simple.html"
  putStrLn $ show $ P.test P.templateParser source
  source <- readFile "./test-data/blog.schema.js"
  putStrLn $ show $ P.test SP.schemaParser source
  putStrLn $ show $ (fmap (S.check ["date", "type"]) $ P.test SP.schemaParser source)

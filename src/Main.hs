module Main where

import qualified Parser as P
import qualified Schema.Parser as SP

main :: IO ()
main = do
  source <- readFile "./test-data/simple.html"
  putStrLn $ show $ P.test P.templateParser source
  source <- readFile "./test-data/blog.schema.js"
  putStrLn $ show $ P.test SP.schemaParser source

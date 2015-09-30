module Main where

import qualified Parser as P

main :: IO ()
main = do
  source <- readFile "./test-data/simple.html"
  putStrLn $ show $ P.test P.fileParser source

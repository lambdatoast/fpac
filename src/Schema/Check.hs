module Schema.Check (check) where

import Schema.Parser

type PropName = String

check :: [PropName] -> Schema -> Bool
check [] _ = True
check _ (Schema []) = False
check (x:xs) (Schema ((Prop y v):ys)) =
  case v of
    SVal _            -> if y == x then True else check (x:xs) (Schema ys)
    OVal props        -> if y == x then check xs (Schema props) else check (x:xs) (Schema ys)
    AVal (SVal _)     -> y == x
    AVal (OVal props) -> if y == x then check xs (Schema props) else False
    AVal (AVal v')     -> check (x:xs) (Schema ((Prop y v'):ys))

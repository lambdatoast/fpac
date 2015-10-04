module Schema.Check (check) where

import Schema.Parser

type Accesor = String

check :: [Accesor] -> Schema -> Bool
check [] _ = True
check _ (Schema []) = False
check (a:as) (Schema ((Prop k v):ps)) =
  case v of
    SVal _            -> if k == a then True else check (a:as) (Schema ps)
    OVal props        -> if k == a then check as (Schema props) else check (a:as) (Schema ps)
    AVal (SVal _)     -> case as of
                           [] -> k == a
                           _  -> False -- trying to further access an SVal
    AVal (OVal props) -> if k == a then check as (Schema props) else check (a:as) (Schema ps)
    AVal (AVal v')     -> check (a:as) (Schema ((Prop k v'):ps))

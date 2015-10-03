module Schema.AST (Val (..), Prop (..), Schema (..)) where

data Val = SVal String | OVal [Prop] | AVal Val deriving (Show)
data Prop = Prop String Val deriving (Show)
data Schema = Schema [Prop] deriving (Show)

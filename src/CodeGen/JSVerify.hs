module CodeGen.JSVerify where

import Schema.AST (Schema(..), Prop(..), Val(..))

import qualified Data.List as L

data Arb = Number_
         | Bool_
         | DateTime_
         | String_
         deriving Show

newtype Name = Name Text deriving Show
data ArbProp = ArbProp Name Arb deriving Show
type Props = [ArbProp]

fromVal :: Val -> Arb
fromVal (SVal "String")  = String_
fromVal (SVal "Boolean") = Bool_
-- data Prop = Prop String Val deriving (Show)
-- date: { type: Date, default: Date.now }
fromVal (OVal ((Prop "type" (SVal "Date")):_)) = DateTime_

generate :: Schema -> Props
generate (Schema []) = []
generate (Schema ((Prop name v):rest)) = (ArbProp (Name name) $ fromVal v) : generate (Schema rest)

type Text = String

fromArb :: Arb -> Text
fromArb Number_   = "jsc.number"
fromArb String_   = "jsc.string"
fromArb Bool_     = "jsc.bool"
fromArb DateTime_ = "jsc.datetime"

showJS :: Props -> Text
showJS arbDef = L.concat $ L.intersperse sep (showJS_ arbDef)
  where
    sep :: Text
    sep = ",\n"
    -- inBrackets :: Text -> Text
    -- inBrackets s = "{" ++ s ++ "}"
    showKV :: Name -> Text -> Text
    showKV (Name n) v = n ++ ": " ++ v
    showJS_ :: Props -> [Text]
    showJS_ [] = []
    showJS_ ((ArbProp name arb):props) = showKV name (fromArb arb) : showJS_ props

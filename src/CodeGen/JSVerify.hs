module CodeGen.JSVerify where

import Schema.AST (Schema(..), Prop(..), Val(..))

import qualified Data.List as L

data Arb = Number_
         | Bool_
         | DateTime_
         | String_
         | JSON_
         | Array_ Arb
         | Record_ [ArbProp]
         deriving Show

newtype Name = Name Text deriving Show
data ArbProp = ArbProp Name Arb deriving Show
type Props = [ArbProp]

recordField :: Prop -> ArbProp
recordField (Prop s v) = ArbProp (Name s) (fromVal v)

fromVal :: Val -> Arb
fromVal (SVal "Number") = Number_
fromVal (SVal "Boolean") = Bool_
fromVal (SVal "Date")  = DateTime_
fromVal (SVal "String")  = String_
fromVal (SVal "Object") = JSON_
fromVal (AVal v) = Array_ $ fromVal v
fromVal (OVal ((Prop "type" (SVal "Date")):_)) = DateTime_
fromVal (OVal props) = Record_ $ fmap recordField props

generate :: Schema -> Props
generate (Schema []) = []
generate (Schema ((Prop name v):rest)) = (ArbProp (Name name) $ fromVal v) : generate (Schema rest)

type Text = String

fromArb :: Arb -> Text
fromArb Number_            = "jsc.number"
fromArb String_            = "jsc.string"
fromArb Bool_              = "jsc.bool"
fromArb DateTime_          = "jsc.datetime"
fromArb (Record_ arbprops) = "jsc.record" ++ (parens . showProps) arbprops
fromArb (Array_ arb)       = "jsc.array" ++ (parens . fromArb) arb
fromArb JSON_              = "jsc.json"

braces :: Text -> Text
braces s = "{" ++ s ++ "}"

parens :: Text -> Text
parens s = "(" ++ s ++ ")"

showProps :: Props -> Text
showProps arbDef = braces $ L.concat $ L.intersperse sep (showJS_ arbDef)
  where
    sep :: Text
    sep = ","
    showKV :: Name -> Text -> Text
    showKV (Name n) v = n ++ ": " ++ v
    showJS_ :: Props -> [Text]
    showJS_ [] = []
    showJS_ ((ArbProp name arb):props) = showKV name (fromArb arb) : showJS_ props

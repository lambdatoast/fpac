module CodeGen.JSVerify where

import Schema.AST

import qualified Data.List as L

data Arb = Number_
         | Bool_
         | DateTime_
         | String_
         deriving Show

newtype Name = Name String deriving Show
data ArbProp = ArbProp Name Arb deriving Show
type Props = [ArbProp]

generate :: Schema -> Props
generate (Schema []) = []
generate (Schema ((Prop name (SVal "String")):rest)) = (ArbProp (Name name) String_) : generate (Schema rest)
generate (Schema ((Prop name (SVal "Boolean")):rest)) = (ArbProp (Name name) Bool_) : generate (Schema rest)

type Output = String

showJS :: Props -> Output
showJS arbDef = L.concat $ L.intersperse sep (showJS_ arbDef)
  where
    sep :: Output
    sep = ",\n"
    inBrackets :: Output -> Output
    inBrackets s = "{" ++ s ++ "}"
    showJS_ :: Props -> [String]
    showJS_ [] = []
    showJS_ ((ArbProp (Name n) Number_):props) = (n ++ ": " ++ "jsc.number") : showJS_ props
    showJS_ ((ArbProp (Name n) String_):props) = (n ++ ": " ++ "jsc.string") : showJS_ props
    showJS_ ((ArbProp (Name n) Bool_):props)   = (n ++ ": " ++ "jsc.bool")   : showJS_ props


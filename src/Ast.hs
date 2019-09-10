{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Ast 
     (

     ) where

data RheaType =
    TBool
    | TNumber
    | TString
    | TGeneric 
    | TStruct 
    | TAbstract  
    | TDict RheaType RheaType 
    | TList RheaType
    | TFunc [RheaType] RheaType


data Expr (a::RheaType) where 
    TrueLit :: Expr TBool
    FalseLit :: Expr TBool
    NumberLit :: Double -> Expr TNumber 
    StringLit :: String -> Expr TString
    Variable :: String -> Expr a
    StructProj :: Expr TStruct -> String -> Expr a
    IndexKey :: Expr (TDict a b) -> Expr a -> Expr b
    FuncCall :: Expr (TFunc input output) -> [Expr a] -> Expr output
    And :: Expr TBool -> Expr TBool -> Expr TBool
    Or :: Expr TBool -> Expr TBool -> Expr TBool
    Xor :: Expr TBool -> Expr TBool -> Expr TBool
    Eq :: Expr TBool -> Expr TBool -> Expr TBool
    Neq :: Expr TBool -> Expr TBool -> Expr TBool
    Not :: Expr TBool -> Expr TBool
    
  

blarg :: Expr TNumber -> Double
blarg x =
    case x of
        NumberLit y -> y

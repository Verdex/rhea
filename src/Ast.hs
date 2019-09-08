{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Ast 
     (

     ) where

data Type =
    TBool
    | TNumber
    | TString
    | TGeneric 
    | TStruct 
    | TAbstract  
    | TDict Type Type 
    | TList Type


data Expr (a::Type) where 
    TrueLit :: Expr TBool
    FalseLit :: Expr TBool
    NumberLit :: Double -> Expr TNumber 
    StringLit :: String -> Expr TString
    Symbol :: String -> Expr a
    Dot :: Expr TStruct -> String -> Expr a
    IndexKey :: Expr (TDict a b) -> Expr a -> Expr b
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

{-# LANGUAGE NoMonomorphismRestriction #-}

module Lib
    ( constModule
    ) where

import GHC.SourceGen (BVar (bvar), HsModule', Var (var), funBind, match,
                      module', typeSig, wildP, (-->))

constModule :: HsModule'
constModule = module' (Just "Const") (Just [var "const"]) [] [constFuncSig, constFuncDef]
  where
    constFuncSig = typeSig "const" $ a --> b --> a
    constFuncDef = funBind "const" $ match [wildP, x] x
    a = var "a"
    b = var "b"
    x = bvar "x"
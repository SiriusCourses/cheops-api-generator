{-# LANGUAGE RecordWildCards #-}

module Data.ConfigGen.Traverse where

import Control.Monad.Reader       (ReaderT, withReaderT)
import Control.Monad.State.Strict (StateT)
import Data.Aeson.KeyMap          (KeyMap)
import Data.ConfigGen.Parsing     (ParserState)
import Data.ConfigGen.TypeRep     (ModuleParts (ModuleParts))
import GHC.SourceGen              (HsDecl', HsType')

type ModulePrefix = [String]

data ConstructedType =
    ConstructedType
        { _constrType :: HsType'
        , _constrDecl :: HsDecl'
        }

type GeneratorState = KeyMap (Either ModuleParts ConstructedType)

type Ctx a = ReaderT ModulePrefix (StateT ParserState (Either String)) a

meh :: ModuleParts -> Ctx [HsDecl']
meh ModuleParts {..} = do
    return undefined

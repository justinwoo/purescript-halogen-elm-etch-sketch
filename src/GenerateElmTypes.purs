module GenerateElmTypes where

import Prelude

import Control.Monad.Aff (Canceler, launchAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Generic.Rep (class Generic, Constructor, Field, Product, Rec)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Main (class IsElmPortSafe, Coords, ElmModel)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Aff (writeTextFile)
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))

getElmRep :: forall a rep
  . Generic a rep
  => IsElmPortSafe rep
  => HasElmRep rep
  => Proxy a
  -> String
getElmRep _ = toElmRep (Proxy :: Proxy rep)

class HasElmRep f where
  toElmRep :: Proxy f -> String

instance herConstructor ::
  ( IsSymbol name
  , HasElmRep inner
  ) => HasElmRep (Constructor name inner) where
  toElmRep _ = "type alias " <> name <> " =" <> contents
    where
      name = reflectSymbol (SProxy :: SProxy name)
      contents = toElmRep (Proxy :: Proxy inner)

instance herRec ::
  ( HasElmRep inner
  ) => HasElmRep (Rec inner) where
  toElmRep _ = "\n  { " <> contents <> "\n  }\n"
    where
      contents = toElmRep (Proxy :: Proxy inner)

instance herProduct ::
  ( HasElmRep a
  , HasElmRep b
  ) => HasElmRep (Product a b) where
  toElmRep _ = first <> "\n  , " <> second
    where
      first = toElmRep (Proxy :: Proxy a)
      second = toElmRep (Proxy :: Proxy b)

instance herField ::
  ( IsSymbol name
  , ExtractName a
  ) => HasElmRep (Field name a) where
  toElmRep _ = name <> " : " <> prop
    where
      name = reflectSymbol (SProxy :: SProxy name)
      prop = extractName (Proxy :: Proxy a)

class ExtractName f where
  extractName :: Proxy f -> String

instance epInt :: ExtractName Int where
  extractName _ = "Int"

instance epArray :: ExtractName a => ExtractName (Array a) where
  extractName _ = "List " <> extractName (Proxy :: Proxy a)

instance epZZZ :: -- overlapping instance because i am a madman
  ( Generic a rep
  , TypeEquals rep (Constructor name b)
  , IsSymbol name
  ) => ExtractName a where
  extractName _ = reflectSymbol (SProxy :: SProxy name)

prepareContents :: String -> String
prepareContents contents = "module EtchSketch.Types exposing (..)\n\n" <> contents

main :: forall e.
  Eff
    ( exception :: EXCEPTION
    , console :: CONSOLE
    , fs :: FS
    | e
    )
    (Canceler
       ( console :: CONSOLE
       , fs :: FS
       | e
       )
    )
main = launchAff do
  writeTextFile UTF8 "./src/EtchSketch/Types.elm" contents
  log "done"
  where
    contents = prepareContents $
      getElmRep (Proxy :: Proxy Coords) <>
        getElmRep (Proxy :: Proxy ElmModel)

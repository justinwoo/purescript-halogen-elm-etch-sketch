module GenerateElmTypes where

import Prelude

import Control.Monad.Aff (Canceler, launchAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Foldable (intercalate)
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Main (class HasElmPortVersion, Coords, EtchSketch)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Aff (writeTextFile)
import Type.Proxy (Proxy(..))
import Type.Row (class ListToRow, class RowToList, Cons, Nil, kind RowList)

getElmRep :: forall a
  . HasElmPortVersion a
  => HasElmTypeRep a
  => Proxy a
  -> String
getElmRep _ = toElmTypeRep (Proxy :: Proxy a)

class HasElmTypeRep r where
  toElmTypeRep :: Proxy r -> String

instance hetrInt :: HasElmTypeRep Int where
  toElmTypeRep _ = "Int"

instance hetrArray :: HasElmTypeName inner => HasElmTypeRep (Array inner) where
  toElmTypeRep _ = "List " <> getElmTypeName (Proxy :: Proxy inner)

instance hetrCoords ::
  ( Newtype Coords rec
  , HasElmTypeRep rec
  ) => HasElmTypeRep Coords where
  toElmTypeRep _ = toElmTypeRep (Proxy :: Proxy rec)

instance hetrRecord ::
  ( HasElmTypeRepFields fieldList
  , RowToList fields fieldList
  ) => HasElmTypeRep (Record fields) where
  toElmTypeRep proxy = "\n  { " <> contents <> "\n  }\n"
    where
      contents = intercalate "\n  , " $ extractFields proxy

class HasElmTypeRepFields (xs :: RowList) where
  extractFields :: forall fields
    . RowToList fields xs
    => Proxy (Record fields)
    -> Array String
instance hetrfCons ::
  ( IsSymbol name
  , HasElmTypeName ty
  , ListToRow tail tailRow
  , HasElmTypeRepFields tail
  , RowToList tailRow tail
  ) => HasElmTypeRepFields (Cons name ty tail) where
  extractFields _ = [name <> " : " <> tyName] <>
    extractFields (Proxy :: Proxy (Record tailRow))
    where
      name = reflectSymbol (SProxy :: SProxy name)
      tyName = getElmTypeName (Proxy :: Proxy ty)
instance hetrfNil :: HasElmTypeRepFields Nil where
  extractFields _ = []

class HasElmTypeName t where
  getElmTypeName :: Proxy t -> String

instance hetnCoords :: HasElmTypeName Coords where
  getElmTypeName _ = "Coords"

-- too lazy to do this manually so full yolo is in order
-- the elm compiler will throw if something's messed up here anyway
instance hetnZZZ :: HasElmTypeRep a => HasElmTypeName a where
  getElmTypeName = toElmTypeRep

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
    elmModel = "type alias ElmModel =" <> getElmRep (Proxy :: Proxy EtchSketch)
    coords = "type alias Coords =" <> getElmRep (Proxy :: Proxy Coords)
    contents = prepareContents $
      coords <> elmModel
      

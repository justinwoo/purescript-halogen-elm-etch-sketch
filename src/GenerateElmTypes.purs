module GenerateElmTypes where

import Prelude

import Control.Monad.Aff (launchAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Data.Newtype (class Newtype)
import Kancho (class HasElmPortVersion, getElmRep)
import Main (Coords, EtchSketch)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (FS, writeTextFile)
import Type.Prelude (Proxy(..))

prepareContents :: String -> String
prepareContents contents = "module EtchSketch.Types exposing (..)\n\n" <> contents

main :: forall t. Eff ( fs :: FS, console :: CONSOLE | t) Unit
main = launchAff_ do
  writeTextFile UTF8 path contents
  log $ "generated elm types to " <> path
  where
    path = "./src/EtchSketch/Types.elm"
    elmModel = "type alias ElmModel = " <> getElmRep (Proxy :: Proxy EtchSketch)
    coords = "type alias Coords = " <> getElmRep coordsRecProxy
    contents = prepareContents $
      coords <> "\n" <> elmModel

    coordsRecProxy :: forall rec
       . Newtype Coords rec
      => HasElmPortVersion rec
      => Proxy rec
    coordsRecProxy = Proxy

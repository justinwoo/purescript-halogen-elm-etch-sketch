module GenerateElmTypes where

import Prelude

import Control.Monad.Aff (launchAff_)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Kancho.Generate (getElmRep)
import Main (Coords, EtchSketch)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Aff (writeTextFile)
import Type.Prelude (Proxy(..))

prepareContents :: String -> String
prepareContents contents = "module EtchSketch.Types exposing (..)\n\n" <> contents

main :: forall e.
  Eff
    ( exception :: EXCEPTION
    , console :: CONSOLE
    , fs :: FS
    | e
    )
    Unit
main = launchAff_ do
  writeTextFile UTF8 path contents
  log $ "generated elm types to " <> path
  where
    path = "./src/EtchSketch/Types.elm"
    elmModel = "type alias ElmModel = " <> getElmRep (Proxy :: Proxy EtchSketch)
    coords = "type alias Coords = " <> getElmRep (Proxy :: Proxy Coords)
    contents = prepareContents $
      coords <> "\n" <> elmModel

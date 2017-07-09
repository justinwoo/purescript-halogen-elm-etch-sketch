module EtchSketch.Types exposing (..)

type alias Coords =
  { x : Int
  , y : Int
  }
type alias ElmModel =
  { cursor : Coords
  , height : Int
  , increment : Int
  , points : List Coords
  , width : Int
  }

module EtchSketch.Types exposing (..)

type alias Coords = 
  { x : Int
  , y : Int
  }
type alias ElmModel = 
  { cursor : 
    { x : Int
    , y : Int
    }
  , height : Int
  , increment : Int
  , points : List 
      { x : Int
      , y : Int
      }
  , width : Int
  }
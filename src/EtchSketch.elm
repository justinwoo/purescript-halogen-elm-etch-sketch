port module Main exposing (..)

import Html exposing (Html, div, button, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Svg exposing (svg, rect)
import Svg.Attributes as SvgAttrs

port clearScreen : () -> Cmd msg
port modelUpdates : (Model -> msg) -> Sub msg

type alias Coords
  = { x : Int, y : Int}

type alias Model =
  { cursor : Coords
  , points : List Coords
  , width : Int
  , height : Int
  , increment : Int
  }

type Msg
  = UpdateModel Model
  | ClearScreen

init : Model
init =
  { cursor = Coords 0 0
  , points = []
  , width = 800
  , height = 600
  , increment = 10
  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateModel newModel ->
      ( newModel, Cmd.none )
    ClearScreen ->
      ( model, clearScreen () )

point : Int -> String -> Coords -> Html Msg
point increment subkey {x, y} =
  rect
    [ SvgAttrs.width <| toString increment
    , SvgAttrs.height <| toString increment
    , SvgAttrs.x <| toString <| x * increment
    , SvgAttrs.y <| toString <| y * increment
    ]
    []

view : Model -> Html Msg
view model =
  let
    newPoint =
      point model.increment

    cursor =
      newPoint "cursor" model.cursor

    points =
      List.map (newPoint "point") model.points
  in
    div
      []
      [ div
          []
          [ button
              [ onClick ClearScreen ]
              [ text "Clear" ]
          ]
      , div
          []
          [ svg
              [ style [ ( "border", "1px solid black" ) ]
              , SvgAttrs.width <| toString model.width
              , SvgAttrs.height <| toString model.height
              ]
              <| cursor
              :: points
          ]
      ]

main : Program Never Model Msg
main =
  Html.program
    { init = ( init, Cmd.none )
    , update = update
    , view = view
    , subscriptions = (\_ -> modelUpdates UpdateModel)
    }
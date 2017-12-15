port module Main exposing (..)

import EtchSketch.Types exposing (..)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Svg exposing (rect, svg)
import Svg.Attributes as SvgAttrs


port clearScreen : () -> Cmd msg


port modelUpdates : (ElmModel -> msg) -> Sub msg


type Msg
    = UpdateModel ElmModel
    | ClearScreen


init : ElmModel
init =
    { cursor = Coords 0 0
    , points = []
    , width = 800
    , height = 600
    , increment = 10
    }


update : Msg -> ElmModel -> ( ElmModel, Cmd Msg )
update msg model =
    case msg of
        UpdateModel newModel ->
            ( newModel, Cmd.none )

        ClearScreen ->
            ( model, clearScreen () )


point : Int -> String -> String -> Coords -> Html Msg
point increment subkey color { x, y } =
    rect
        [ SvgAttrs.width <| toString increment
        , SvgAttrs.height <| toString increment
        , SvgAttrs.x <| toString <| x * increment
        , SvgAttrs.y <| toString <| y * increment
        , SvgAttrs.fill <| color
        ]
        []


view : ElmModel -> Html Msg
view model =
    let
        newPoint =
            point model.increment

        cursor =
            newPoint "cursor" "red" model.cursor

        points =
            List.map (newPoint "point" "black") model.points
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
              <|
                cursor
                    :: points
            ]
        ]


main : Program Never ElmModel Msg
main =
    Html.program
        { init = ( init, Cmd.none )
        , update = update
        , view = view
        , subscriptions = \_ -> modelUpdates UpdateModel
        }

module Main where

import Prelude

import Data.Array (insert)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (error, log)
import FRP.Event (Event, subscribe)
import FRP.Event.Keyboard as Keyboard
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Halogen.VDom.Driver as D
import Kancho (class HasElmPortVersion, toElmModel)
import Web.HTML (HTMLElement)

foreign import data ElmInstance :: Type
foreign import getElmInstance :: HTMLElement -> Effect ElmInstance
foreign import subscribeToClearScreen_ :: ElmInstance -> (Unit -> Effect Unit) -> Effect Unit
foreign import sendModelUpdate :: ElmInstance -> EtchSketch -> Effect Unit

data Direction
  = Up
  | Down
  | Left
  | Right
  | UpRight
  | UpLeft
  | DownRight
  | DownLeft

newtype Coords = Coords
  { x :: Int
  , y :: Int
  }
derive instance ntCoords :: Newtype Coords _
instance hepvCoords :: HasElmPortVersion Coords where
  toElmTypeRep _ = "Coords"
derive instance eqCoords :: Eq Coords
derive instance ordCoords :: Ord Coords

type EtchSketch =
  { cursor :: Coords
  , points :: Array Coords
  , height :: Int
  , width :: Int
  , increment :: Int
  }

type State =
  { elmInstance :: Maybe ElmInstance
  , etchSketch :: EtchSketch
  }

isInvalidPoint :: State -> Coords -> Boolean
isInvalidPoint {etchSketch: {increment, width, height}} (Coords {x, y})
  | x < 0 = true
  | y < 0 = true
  | x > width / increment - 1 = true
  | y > height / increment - 1 = true
  | otherwise = false

moveCursor :: Direction -> State -> State
moveCursor direction state@{etchSketch: {cursor: (Coords coords@{x, y})}} =
  if isInvalidPoint state cursor'
    then state
    else state {etchSketch {cursor = cursor', points = points'}}
  where
    points' = insert state.etchSketch.cursor state.etchSketch.points
    cursor' = case direction of
      Up -> Coords coords {y = (y - 1)}
      Down -> Coords coords {y = (y + 1)}
      Left -> Coords coords {x = (x - 1)}
      Right -> Coords coords {x = (x + 1)}
      UpRight -> Coords coords {x = (x + 1), y = (y - 1)}
      UpLeft -> Coords coords {x = (x - 1), y = (y - 1)}
      DownRight -> Coords coords {x = (x + 1), y = (y + 1)}
      DownLeft -> Coords coords {x = (x - 1), y = (y + 1)}


data Query a
  = Init a
  | MoveCursor Direction a
  | ClearScreen Unit (H.SubscribeStatus -> a)
  | UpdateElm a

ui :: H.Component HH.HTML Query Unit Void Aff
ui =
  H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    }
  where
    initialState :: State
    initialState =
      { elmInstance: Nothing
      , etchSketch:
          { cursor: Coords {x: 0, y: 0}
          , points: mempty
          , width: 800
          , height: 600
          , increment: 10
          }
      }

    render :: State -> H.ComponentHTML Query
    render state =
      HH.div
        [ HP.class_ $ HH.ClassName "ui-root"
        ,  HP.ref rootLabel
        ]
        []

    eval :: Query ~> H.ComponentDSL State Query Void Aff
    eval (Init next) = do
      root <- H.getHTMLElementRef rootLabel
      case root of
        Just element -> do
          elmInstance <- H.liftEffect $ getElmInstance element
          H.subscribe $ ES.eventSource (subscribeToClearScreen_ elmInstance)
            (Just <<< H.request <<< ClearScreen)
          H.modify_ _ { elmInstance = Just elmInstance }
        Nothing -> do
          error' "Couldn't get root instance"
      eval (UpdateElm next)

    eval (MoveCursor direction next) = do
      H.modify_ $ moveCursor direction
      eval (UpdateElm next)

    eval (ClearScreen _ reply) = do
      H.modify_ _ {etchSketch {points = mempty :: Array Coords}}
      _ <- eval (UpdateElm reply)
      pure (reply H.Listening)

    eval (UpdateElm next) = do
      state <- H.get
      case state.elmInstance of
        Just elmInstance -> do
          pure unit
          H.liftEffect $ sendModelUpdate elmInstance
            (toElmModel $ state.etchSketch)
        Nothing ->
          pure unit
      pure next

    rootLabel = H.RefLabel "root"
    error' = H.liftAff <<< error

directions :: Event (Maybe Direction)
directions = do
  toDirection <$> Keyboard.down
  where
    toDirection "ArrowUp" = Just Up
    toDirection "ArrowDown" = Just Down
    toDirection "ArrowLeft" = Just Left
    toDirection "ArrowRight" = Just Right
    toDirection _ = Nothing

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- D.runUI ui unit body

  _ <- liftEffect $ subscribe directions
    case _ of
      Just a -> do
        void <<< launchAff <<< io.query $
          H.action (MoveCursor a)
      Nothing -> pure unit

  log "Running"

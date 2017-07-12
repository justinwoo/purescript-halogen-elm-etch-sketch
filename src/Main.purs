module Main where

import Prelude

import Control.Monad.Aff (Aff, launchAff, liftEff')
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (CONSOLE, error, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)
import DOM.HTML.Types (HTMLElement)
import Data.Array (insert)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Newtype (class Newtype)
import Data.Set (member)
import FRP (FRP)
import FRP.Behavior (Behavior, animate)
import FRP.Behavior.Keyboard (keys)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Halogen.VDom.Driver as D
import Type.Row (class RowToList, Cons, Nil, kind RowList)

toElmModel :: forall a
  . HasElmPortVersion a
  => a
  -> a
toElmModel = id

class HasElmPortVersion ty
instance hepvInt :: HasElmPortVersion Int
instance hepvString :: HasElmPortVersion String
instance hepvBoolean :: HasElmPortVersion Boolean
instance hepvArray :: HasElmPortVersion inner => HasElmPortVersion (Array inner)
instance hepvRecord ::
  ( RowToList fields fieldList
  , CheckElmPortVersionFields fieldList
  ) => HasElmPortVersion (Record fields)

class CheckElmPortVersionFields (xs :: RowList)
instance cepvfCons ::
  ( HasElmPortVersion ty
  , CheckElmPortVersionFields tail
  ) => CheckElmPortVersionFields (Cons name ty tail)
instance cepvfNil :: CheckElmPortVersionFields Nil

foreign import data ElmInstance :: Type
foreign import getElmInstance :: forall eff.
  HTMLElement
  -> Eff (dom :: DOM | eff) ElmInstance
foreign import subscribeToClearScreen_ :: forall eff.
  ElmInstance
  -> (Unit -> Eff eff Unit)
  -> Eff eff Unit
foreign import sendModelUpdate :: forall eff.
  ElmInstance
  -> EtchSketch
  -> Eff eff Unit

data Direction
  = Up
  | Down
  | Left
  | Right

newtype Coords = Coords
  { x :: Int
  , y :: Int
  }
derive instance ntCoords :: Newtype Coords _
instance hepvCoords ::
  ( Newtype Coords rec
  , HasElmPortVersion rec
  ) => HasElmPortVersion Coords
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

data Query a
  = Init a
  | MoveCursor Direction a
  | ClearScreen Unit (H.SubscribeStatus -> a)
  | UpdateElm a

type AppEffects eff =
  ( console :: CONSOLE
  , dom :: DOM
  , frp :: FRP
  , avar :: AVAR
  | eff
  )

type AppAff eff = Aff (AppEffects eff)

ui :: forall eff. H.Component HH.HTML Query Unit Void (AppAff eff)
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

    eval :: Query ~> H.ComponentDSL State Query Void (AppAff eff)
    eval (Init next) = do
      root <- H.getHTMLElementRef rootLabel
      case root of
        Just element -> do
          elmInstance <- H.liftEff $ getElmInstance element
          H.subscribe $ ES.eventSource (subscribeToClearScreen_ elmInstance)
            (Just <<< H.request <<< ClearScreen)
          H.modify _ { elmInstance = Just elmInstance }
        Nothing -> do
          error' "Couldn't get root instance"
      eval (UpdateElm next)

    eval (MoveCursor direction next) = do
      H.modify $ moveCursor direction
      eval (UpdateElm next)

    eval (ClearScreen _ reply) = do
      H.modify _ {etchSketch {points = mempty :: Array Coords}}
      _ <- eval (UpdateElm reply)
      pure (reply H.Listening)

    eval (UpdateElm next) = do
      state <- H.get
      case state.elmInstance of
        Just elmInstance -> do
          pure unit
          H.liftEff $ sendModelUpdate elmInstance
            (toElmModel $ state.etchSketch)
        Nothing ->
          pure unit
      pure next

    rootLabel = H.RefLabel "root"
    error' = H.liftAff <<< error

directions :: Behavior (Maybe Direction)
directions = do
  toDirection <$> keys
  where
    toDirection set
      | member 38 set = Just Up
      | member 40 set = Just Down
      | member 37 set = Just Left
      | member 39 set = Just Right
      | otherwise = Nothing

main :: forall e.
  Eff
    ( AppEffects
        ( avar :: AVAR
        , ref :: REF
        , exception :: EXCEPTION
        | e
        )
    )
    Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- D.runUI ui unit body

  _ <- liftEff' $ animate directions
    case _ of
      Just a -> do
        void <<< launchAff <<< io.query $
          H.action (MoveCursor a)
      Nothing -> pure unit

  log "Running"
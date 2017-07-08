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
import Data.Array (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Set (Set, insert, member)
import FRP (FRP)
import FRP.Behavior (Behavior, animate)
import FRP.Behavior.Keyboard (keys)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Halogen.VDom.Driver as D

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
  -> ElmModel
  -> Eff eff Unit

type ElmCoords =
  { x :: Int
  , y :: Int
  }

type ElmModel =
  { cursor :: ElmCoords
  , points :: Array ElmCoords
  , width :: Int
  , height :: Int
  , increment :: Int
  }

stateToElmModel :: State -> ElmModel
stateToElmModel state =
  { cursor: coordsToElmCoords state.cursor
  , points: coordsToElmCoords <$> fromFoldable state.points
  , width: state.width
  , height: state.height
  , increment: state.increment
  }
  where
    coordsToElmCoords (Coords x y) = { x, y }

data Direction
  = KUp
  | KDown
  | KLeft
  | KRight

data Coords = Coords Int Int
derive instance eqCoords :: Eq Coords
derive instance ordCoords :: Ord Coords

type State =
  { elmInstance :: Maybe ElmInstance
  , cursor :: Coords
  , points :: Set Coords
  , height :: Int
  , width :: Int
  , increment :: Int
  }

isInvalidPoint :: State -> Coords -> Boolean
isInvalidPoint {increment, width, height} (Coords x y)
  | x < 0 = true
  | y < 0 = true
  | x > width / increment - 1 = true
  | y > height / increment - 1 = true
  | otherwise = false

moveCursor :: Direction -> State -> State
moveCursor direction state@{cursor: (Coords x y)} =
  if isInvalidPoint state cursor'
    then state
    else state {cursor = cursor', points = points'}
  where
    points' = insert state.cursor state.points
    cursor' = case direction of
      KUp -> Coords x (y - 1)
      KDown -> Coords x (y + 1)
      KLeft -> Coords (x - 1) y
      KRight -> Coords (x + 1) y

data Query a
  = Init a
  | MoveCursor Direction a
  | ClearScreen Unit (H.SubscribeStatus -> a)
  | UpdateElm a

data Message = ElmInstanceInitialized ElmInstance

type AppEffects eff =
  ( console :: CONSOLE
  , dom :: DOM
  , frp :: FRP
  , avar :: AVAR
  | eff
  )

type AppAff eff = Aff (AppEffects eff)

ui :: forall eff. H.Component HH.HTML Query Unit Message (AppAff eff)
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
      , cursor: Coords 0 0
      , points: mempty
      , width: 800
      , height: 600
      , increment: 10
      }

    render :: State -> H.ComponentHTML Query
    render state =
      HH.div
        [ HP.class_ $ HH.ClassName "ui-root"
        ,  HP.ref rootLabel
        ]
        []

    eval :: Query ~> H.ComponentDSL State Query Message (AppAff eff)
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
      H.modify _ { points = mempty :: Set Coords }
      _ <- eval (UpdateElm reply)
      pure (reply H.Listening)

    eval (UpdateElm next) = do
      state <- H.get
      case state.elmInstance of
        Just elmInstance -> do
          H.liftEff $ sendModelUpdate elmInstance (stateToElmModel state)
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
      | member 38 set = Just KUp
      | member 40 set = Just KDown
      | member 37 set = Just KLeft
      | member 39 set = Just KRight
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
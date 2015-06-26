module Main where

import Control.Alt
import Control.Apply
import Control.Monad.Eff (Eff(..))
import Data.Function
import Data.Maybe
import Debug.Trace
import qualified Rx.Observable as Rx

import Core
import KeyCodes
import Types
import qualified UI.React as UIReact
import qualified UI.Canvas as UICanvas
import qualified UI.Console as UIConsole
import Utils


main = do
  uiParam <- getParameterByName "ui"
  initialState <- getInitialState
  stateStream <- scan (updateStateFactory ticksPlayPauseStream) initialState jointActionsStream

  case uiParam of
      "react"        -> setupReact initialState stateStream
      "canvas"       -> setupCanvas initialState stateStream
      "console"      -> setupConsole initialState stateStream
      "react_canvas" -> (setupReact initialState stateStream) *> (setupCanvas initialState stateStream)

      _              -> setupCanvas initialState stateStream

  rawKeysStream <- fromEvent "keyup"
  (keyEventToKeyCode <$> rawKeysStream) `Rx.subscribe` keyCommand -- TODO move this to UIs, or/and dedicated input component

  pure $ onNext ticksPlayPauseStream true

  where
  setupReact initialState stateStream = void $ do
    vStream <- UIReact.setupUI initialState actionsStream "root_layout"
    stateStream `Rx.subscribe` (void <<< pure <<< onNext vStream)

  setupCanvas initialState stateStream = void $ do
    vStream <- UICanvas.setupUI initialState actionsStream "canvas"
    stateStream `Rx.subscribe` (void <<< pure <<< onNext vStream)

  setupConsole initialState stateStream = void $ do
    vStream <- UIConsole.setupUI initialState actionsStream ""
    stateStream `Rx.subscribe` (void <<< pure <<< onNext vStream)

  timerStream = (\_ -> Timer) <$> (getIntervalStream 1000)

  ticksStream = (\_ -> Tick) <$> (getIntervalStream initialSpeed)
  pausableTicksStream = pausable ticksStream ticksPlayPauseStream

  actionsStream = runFn0 newSubject
  ticksPlayPauseStream = runFn0 newSubject

  jointActionsStream = pausableTicksStream <|> actionsStream <|> timerStream

  keyToAction :: KeyCode -> Maybe Action
  keyToAction Space      = Just Toggle
  keyToAction LeftArrow  = Just $ Rewind 1
  keyToAction RightArrow = Just $ FForward 1
  keyToAction _          = Nothing

  keyCommand :: forall eff. KeyCode -> Eff eff Unit
  keyCommand key = case keyToAction key of
      Just action -> void <<< pure <<< onNext actionsStream $ action
      Nothing     -> pure unit


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

  case uiParam of
      "react"        -> setupReact
      "canvas"       -> setupCanvas
      "console"      -> setupConsole
      "react_canvas" -> setupReact *> setupCanvas

      _              -> setupCanvas

  keysStream `Rx.subscribe` keyCommand -- TODO move this to UIs, or/and dedicated input component

  pure $ onNext playPauseStream true

  where
  setupReact = do
    vStream <- UIReact.setupUI initialState actionsStream "root_layout"
    scanStream `Rx.subscribe` (void <<< pure <<< onNext vStream)

  setupCanvas = do
    vStream <- UICanvas.setupUI initialState actionsStream "canvas"
    scanStream `Rx.subscribe` (void <<< pure <<< onNext vStream)

  setupConsole = do
    vStream <- UIConsole.setupUI initialState actionsStream ""
    scanStream `Rx.subscribe` (void <<< pure <<< onNext vStream)

  timerStream = (\_ -> Timer) <$> (getIntervalStream 1000)

  intervalStream = (\_ -> Tick) <$> (getIntervalStream initialSpeed)
  pausableIntervalStream = pausable intervalStream playPauseStream

  actionsStream = runFn0 newSubject
  playPauseStream = runFn0 newSubject

  mainStream = pausableIntervalStream <|> actionsStream <|> timerStream
  scanStream = Rx.scan (updateStateFactory playPauseStream) initialState mainStream

  rawKeysStream = fromEvent "keyup"
  keysStream = keyEventToKeyCode <$> rawKeysStream

  keyToAction :: KeyCode -> Maybe Action
  keyToAction Space      = Just Toggle
  keyToAction LeftArrow  = Just $ Rewind 1
  keyToAction RightArrow = Just $ FForward 1
  keyToAction _          = Nothing

  keyCommand :: forall eff. KeyCode -> Eff eff Unit
  keyCommand key = case keyToAction key of
      Just action -> void <<< pure <<< onNext actionsStream $ action
      Nothing     -> pure unit


module Main where

import Control.Alt
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

  vStream <- case uiParam of
                "react"   -> UIReact.setupUI   initialState actionsStream "root_layout"
                "canvas"  -> UICanvas.setupUI  initialState actionsStream "canvas"
                "console" -> UIConsole.setupUI initialState actionsStream ""

                _         -> UICanvas.setupUI  initialState actionsStream "canvas"

  scanStream `Rx.subscribe` (void <<< pure <<< onNext vStream)
  keysStream `Rx.subscribe` keyCommand

  pure $ onNext playPauseStream true

  where
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


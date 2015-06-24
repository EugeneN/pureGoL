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
import Utils


main = do
  uiParam <- getParameterByName "ui"

  vStream <- case uiParam of
                "react"  -> UIReact.setupUI  initialState actionsStream "root_layout"
                _        -> UICanvas.setupUI initialState actionsStream "canvas"

  scanStream `Rx.subscribe` \s -> void $ pure $ onNext vStream s

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
      Just action -> do
          pure $ onNext actionsStream action
          pure unit
      Nothing -> pure unit


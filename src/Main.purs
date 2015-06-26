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
  stateStream <- scan processState initialState jointActionsStream

  case uiParam of
      "react"        -> setupReact initialState stateStream
      "canvas"       -> setupCanvas initialState stateStream
      "console"      -> setupConsole initialState stateStream
      "react_canvas" -> (setupReact initialState stateStream)
                        *> (setupCanvas initialState stateStream)

      _              -> setupCanvas initialState stateStream

  -- TODO move this to UIs, or/and dedicated input component
  rawKeysStream <- fromEvent "keyup"
  (keyEventToKeyCode <$> rawKeysStream) `Rx.subscribe` keyCommand

  onNext ticksPlayPauseStream true

  where
  setupReact   = setupUI UIReact.setupUI "root_layout"
  setupCanvas  = setupUI UICanvas.setupUI "canvas"
  setupConsole = setupUI UIConsole.setupUI ""

  setupUI ui placeholderId initialState stateStream = void $ do
    viewInputStream <- ui initialState actionsStream placeholderId
    stateStream `Rx.subscribe` (onNext viewInputStream)

  processState = processStateFactory ticksPlayPauseStream

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
      Just action -> onNext actionsStream $ action
      Nothing     -> pure unit

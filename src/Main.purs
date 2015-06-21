module Main where

import Control.Alt
import Control.Monad.Eff (Eff(..))
import Data.Function
import Data.Maybe
import Debug.Trace
import qualified Rx.Observable as Rx

import Core
import Data
import KeyCodes
import Types
import UI
import Utils


main = do
  view <- renderMainView "root_layout" initialState actionsStream
  scanStream `Rx.subscribe` \s -> setProps view { actionsStream: actionsStream, state: s }

  keysStream `Rx.subscribe` keyCommand

  pure $ onNext playPauseStream true

  where
  initialSpeed = 50
  initialState = State { cells: [initialCells]
                       , runningState: Running
                       , current: Nothing
                       , startTime: runFn0 now }

  intervalStream = (\_ -> Tick) <$> (getIntervalStream initialSpeed)
  pausableIntervalStream = pausable intervalStream playPauseStream

  actionsStream = runFn0 newSubject
  playPauseStream = runFn0 newSubject

  mainStream = pausableIntervalStream <|> actionsStream
  scanStream = Rx.scan (updateStateFactory playPauseStream) initialState mainStream

  rawKeysStream = fromEvent "keyup"
  keysStream = keyEventToKeyCode <$> rawKeysStream

  keyToAction :: KeyCode -> Action
  keyToAction Space      = Toggle
  keyToAction LeftArrow  = Rewind 1
  keyToAction RightArrow = FForward 1

  keyCommand :: forall eff. KeyCode -> Eff eff Unit
  keyCommand key = do
    pure $ onNext actionsStream (keyToAction key)
    pure unit


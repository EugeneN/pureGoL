module Main where

import Control.Alt
import Data.Function
import Debug.Trace
import qualified Rx.Observable as Rx

import Core
import Data
import Types
import UI
import Utils


initialSpeed = 50
initialState = State { cells: initialCells
                     , runningState: Running }

main = do
  view <- renderMainView "root_layout" initialState actionsStream
  scanStream ~> \s -> setProps view { actionsStream: actionsStream, state: s }

  pure $ onNext playPauseStream true


  where
  intervalStream = (\_ -> Tick) <$> (getIntervalStream initialSpeed)
  pausableIntervalStream = pausable intervalStream playPauseStream

  actionsStream = runFn0 newSubject
  playPauseStream = runFn0 newSubject

  mainStream = pausableIntervalStream <|> actionsStream
  scanStream = Rx.scan (updateState playPauseStream) initialState mainStream



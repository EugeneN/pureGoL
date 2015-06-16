module Main where

import Control.Alt
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

  actionsStream = newSubject 1
  playPauseStream = newSubject 1

  mainStream = pausableIntervalStream <|> actionsStream
  scanStream = Rx.scan updateState initialState mainStream

  updateState :: Action -> State -> State
  updateState Tick          state = calculateNewGeneration state
  updateState Play          state = play playPauseStream state
  updateState Pause         state = pause playPauseStream state
  updateState Save          state = proxyLog state
  updateState (Point y x)   state = addPoint state y x
  updateState (NoPoint y x) state = removePoint state y x
  updateState (NewCells cs) state = setNewCells state cs

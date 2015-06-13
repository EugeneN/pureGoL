module Main where

import Debug.Trace
import Types
import Utils
import UI
import Data

import Data.Maybe
import Data.Tuple
import Data.Array
import Control.Monad.Eff (Eff(..))
import qualified Rx.Observable as Rx

initialSpeed = 50
initialState = State { cells: initialCells
                     , runningState: Running }

calculateNewGeneration :: State -> State
calculateNewGeneration (State { cells = oldCells
                              , runningState = rs }) = State { cells: calcNewCells oldCells
                                                             , runningState: rs }
  where
  calcNewCells cells =
      map_ (zip cells (0 .. (length cells))) \(Tuple row rowIdx) ->
          map_ (zip row (0 .. (length row))) \(Tuple cell cellIdx) ->
              let neighbours = findNeighbours rowIdx cellIdx cells
                  liveCount = length (filter isAlive neighbours)
              in case cell of
                  Alive -> if liveCount < 2 || liveCount > 3 then Dead else Alive
                  Dead  -> if liveCount == 3 then Alive else Dead

  isAlive :: Cell -> Boolean
  isAlive Alive = true
  isAlive Dead  = false

  findNeighbours :: Number -> Number -> [[Cell]] -> [Cell]
  findNeighbours y x cells = catMaybes maybeNeighbours
    where
      maybeNeighbours = map (\[y, x] -> getByIndex2 cells y x) newCells
      newCells = [ [y-1, x-1], [y,   x-1], [y+1, x-1], [y-1, x]
                 , [y+1, x],   [y-1, x+1], [y,   x+1], [y+1, x+1] ]

togglePoint :: Cell -> State -> Number -> Number -> State
togglePoint cell (State { cells = oldCells
                        , runningState = rs }) y x = State { cells: updateAt2 y x cell oldCells
                                                           , runningState: rs }
addPoint    = togglePoint Alive
removePoint = togglePoint Dead

playHack :: forall a. State -> a -> State
playHack (State s) _ = State (s {runningState = Running })

pauseHack :: forall a. State -> a -> State
pauseHack (State s) _ = State (s {runningState = Paused })

main = do
    view <- renderMainView "root_layout" initialState actionsStream
    scanStream ~> \s -> setProps view { actionsStream: actionsStream, state: s }

    pure $ onNext playPauseStream true


  where
  intervalStream = (\_ -> Interval) <$> (getIntervalStream initialSpeed)
  pausableIntervalStream = pausable intervalStream playPauseStream

  actionsStream = newSubject 1
  playPauseStream = newSubject 1

  mainStream = pausableIntervalStream `Rx.merge` actionsStream
  scanStream = Rx.scan updateState initialState mainStream

  updateState :: Action -> State -> State
  updateState Interval      state = calculateNewGeneration state
  updateState Play          state = playHack  state $ onNext playPauseStream true
  updateState Pause         state = pauseHack state $ onNext playPauseStream false
  updateState Dump          state = proxyLog state
  updateState (Point y x)   state = addPoint state y x
  updateState (NoPoint y x) state = removePoint state y x

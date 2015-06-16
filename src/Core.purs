module Core
  ( calculateNewGeneration
  , addPoint
  , removePoint
  , play
  , pause
  , setNewCells
  ) where

import Control.Apply
import Data.Array
import Data.Maybe.Unsafe (fromJust)
import Data.Maybe
import Data.Tuple
import qualified Rx.Observable as Rx

import Types
import Utils


calculateNewGeneration :: State -> State
calculateNewGeneration (State s) = State (s { cells = calcNewCells s.cells })
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
togglePoint newCell (State s) y x = State (s { cells = updateAt2 y x newCell s.cells })

addPoint    = togglePoint Alive
removePoint = togglePoint Dead

toggle :: Boolean -> RunStatus -> Rx.Observable Boolean -> State -> State
toggle cmd rs playPauseStream  (State s) = fromJust $ (pure $ onNext playPauseStream cmd) *>
                                                      (pure $ State (s {runningState = rs }))
play  = toggle true Running
pause = toggle false Paused

setNewCells :: State -> [[Cell]] -> State
setNewCells (State s) cs = State (s { cells = cs })
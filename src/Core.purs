module Core
  ( calculateNewGeneration
  , addPoint
  , removePoint
  , play
  , pause
  , setNewCells
  , updateState
  ) where

import Control.Apply
import Data.Array
import Data.Maybe.Unsafe (fromJust)
import Data.Maybe
import Data.Tuple
import qualified Rx.Observable as Rx

import Types
import Utils

-- | This is the heart of GoL. It calculates a new generation based on
-- | previous one and the rules.
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

toggle :: Boolean -> RunStatus -> State -> Rx.Observable Boolean -> State
toggle cmd rs (State s) playPauseStream = fromJust $ (pure $ onNext playPauseStream cmd) *>
                                                     (pure $ State (s {runningState = rs }))
play  = toggle true Running
pause = toggle false Paused

setNewCells :: State -> [[Cell]] -> State
setNewCells (State s) cs = State (s { cells = cs })


-- | This is the application's state machine. It maps `Action`s to new `State`s
updateState :: Rx.Observable Boolean ->  Action -> State -> State
updateState _ Tick          state = calculateNewGeneration state
updateState o Play          state = play state o
updateState o Pause         state = pause state o
updateState _ Save          state = proxyLog state
updateState _ (Point y x)   state = addPoint state y x
updateState _ (NoPoint y x) state = removePoint state y x
updateState _ (NewCells cs) state = setNewCells state cs
module Core
  ( calculateNewGeneration
  , addPoint
  , removePoint
  , play
  , pause
  , updateState
  , getTotalGenerations
  , getCurrentGeneration
  , saveNewGeneration
  ) where

import Control.Apply
import Data.Array
import Data.Maybe.Unsafe (fromJust)
import Data.Maybe
import Control.Monad.Eff (runPure)
import Data.Tuple
import qualified Rx.Observable as Rx

import Types
import Utils

getTotalGenerations :: State -> Number
getTotalGenerations (State s) = length s.cells

getCurrentGeneration :: State -> Generation
getCurrentGeneration (State s) = case s.current of
    Nothing  -> maybe [[]] id $ last s.cells
    Just idx -> maybe [[]] id $ s.cells !! idx

saveNewGeneration :: State -> Generation -> State
saveNewGeneration (State s) ng = State (s { cells = snoc s.cells ng })

rewind :: Number -> State -> State
rewind n (State s) =
    let newCurrent = case s.current of
                        Just x  -> x - n
                        Nothing -> (length s.cells) - n
        boundedNewCurrent = if newCurrent < 0 then 0 else newCurrent
    in State (s { current = Just boundedNewCurrent })

fforward :: Number -> State -> State
fforward n (State s) =
    let maxIndex = (length s.cells) - 1
        newCurrent = case s.current of
                        Just x  -> if x + n > maxIndex then Nothing else Just (x + n)
                        Nothing -> Nothing
    in State (s { current = newCurrent })

-- | This is the heart of GoL. It calculates a new generation based on
-- | previous one and the rules.
calculateNewGeneration :: State -> State
calculateNewGeneration state = saveNewGeneration state newGeneration
  where
  currentGeneration = getCurrentGeneration state
  newGeneration = calcNewCells currentGeneration
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

  findNeighbours :: Number -> Number -> Generation -> [Cell]
  findNeighbours y x cells = catMaybes maybeNeighbours
      where
      maybeNeighbours = map (\[y, x] -> getByIndex2 cells y x) newCells
      newCells = [ [y-1, x-1], [y,   x-1], [y+1, x-1], [y-1, x  ]
                 , [y+1, x  ], [y-1, x+1], [y,   x+1], [y+1, x+1] ]

togglePoint :: Cell -> State -> Number -> Number -> State
togglePoint newCell state y x = saveNewGeneration state newGeneration
    where currentGeneration = getCurrentGeneration state
          newGeneration = updateAt2 y x newCell currentGeneration

addPoint    = togglePoint Alive
removePoint = togglePoint Dead

toggle :: Boolean -> RunStatus -> Rx.Observable Boolean -> State -> State
toggle cmd rs playPauseStream (State s) = fromJust $ (pure $ onNext playPauseStream cmd) *>
                                                     (pure $ State (s {runningState = rs }))
play  = toggle true Running
pause = toggle false Paused

-- | This is the application's state machine. It maps `Action`s to new `State`s
updateState :: Rx.Observable Boolean ->  Action -> State -> State
updateState _ Tick          state = calculateNewGeneration state
updateState o Play          state = play o state
updateState o Pause         state = pause o state
updateState _ Save          state = proxyLog state
updateState _ (Point y x)   state = addPoint state y x
updateState _ (NoPoint y x) state = removePoint state y x
updateState _ (NewCells cs) state = saveNewGeneration state cs
updateState o (Rewind n)    state = (pause o >>> rewind n) state
updateState o (FForward n)  state = (pause o >>> fforward n) state
module Core
  ( calculateNewGeneration
  , addPoint
  , removePoint
  , play
  , pause
  , updateStateFactory
  , getTotalGenerations
  , getCurrentGeneration
  , saveNewGeneration
  ) where

import Control.Apply
import Data.Array
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
fforward n state@(State s) = case s.current of
    Just x ->
        let maxIndex = (length s.cells) - 1
            newCurrent = if x + n > maxIndex then Nothing else Just (x + n)
        in State (s { current = newCurrent })

    Nothing ->
        saveNewGeneration state ((getCurrentGeneration >>> genNewGeneration) state)

-- | This is the heart of GoL. It calculates a new generation based on
-- | previous one and the rules.
genNewGeneration :: Generation -> Generation
genNewGeneration currentGeneration = calcNewCells currentGeneration
    where
    calcNewCells cells =
        map_ (zip cells (0 .. (length cells))) \(Tuple row rowIdx) ->
            map_ (zip row (0 .. (length row))) \(Tuple cell cellIdx) ->
                let neighbours = findNeighbours rowIdx cellIdx cells
                    liveCount = length (filter ((==) Alive) neighbours)
                in case cell of
                    Alive -> if liveCount < 2 || liveCount > 3 then Dead else Alive
                    Dead  -> if liveCount == 3 then Alive else Dead

    findNeighbours :: Number -> Number -> Generation -> [Cell]
    findNeighbours y x cells = catMaybes maybeNeighbours
        where
        maybeNeighbours = map (\[y, x] -> getByIndex2 cells y x) newCells
        newCells = [ [y-1, x-1], [y,   x-1], [y+1, x-1], [y-1, x  ]
                   , [y+1, x  ], [y-1, x+1], [y,   x+1], [y+1, x+1] ]

calculateNewGeneration :: State -> State
calculateNewGeneration state = saveNewGeneration state newGeneration
  where
  newGeneration = (getCurrentGeneration >>> genNewGeneration) state

togglePoint :: Cell -> State -> Number -> Number -> State
togglePoint newCell state y x = saveNewGeneration state newGeneration
    where currentGeneration = getCurrentGeneration state
          newGeneration = updateAt2 y x newCell currentGeneration

addPoint    = togglePoint Alive
removePoint = togglePoint Dead

toggleTicks :: RunStatus -> Rx.Observable Boolean -> State -> State
toggleTicks rs playPauseStream (State s) = runPure (do
    pure $ onNext playPauseStream $ case rs of
                                        Running -> true
                                        Paused  -> false
    pure $ State (s {runningState = rs}))

play  = toggleTicks Running
pause = toggleTicks Paused

-- | This is the application's state machine. It maps `Action`s to new `State`s
updateStateFactory :: Rx.Observable Boolean ->  (Action -> State -> State)
updateStateFactory o = updateState
  where
  updateState Tick          state = calculateNewGeneration state
  updateState Play          state = play o state
  updateState Pause         state = pause o state
  updateState Save          state = proxyLog state
  updateState (Point y x)   state = addPoint state y x
  updateState (NoPoint y x) state = removePoint state y x
  updateState (NewCells cs) state = saveNewGeneration state cs
  updateState (Rewind n)    state = (pause o >>> rewind n) state
  updateState (FForward n)  state = (pause o >>> fforward n) state
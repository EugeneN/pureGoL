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
  , initialSpeed
  , getInitialState
  ) where

import Control.Apply
import Data.Array
import Data.Function
import Data.Maybe
import Control.Monad.Eff
import Data.Tuple
import qualified Rx.Observable as Rx
import Data.Date

import Data
import Types
import Utils

emptyGeneration = [[]] :: Generation
initialSpeed = 50

getInitialState :: forall e. Eff (now :: Now | e) State
getInitialState = do
  startTime <- now

  pure $ State { cells: [initialCells]
               , runningState: Running
               , current: Nothing
               , startTime: startTime
               , secondsElapsed: 0
               , genCounter: 0
               , genRatio: 0 }

getTotalGenerations :: State -> Number
getTotalGenerations (State s) = length s.cells

getCurrentGeneration :: State -> Generation
getCurrentGeneration (State s) = case s.current of
    Nothing  -> maybe emptyGeneration id $ last s.cells
    Just idx -> maybe emptyGeneration id $ s.cells !! idx

saveNewGeneration :: State -> Generation -> State
saveNewGeneration (State s) ng = State (s { cells = snoc s.cells ng
                                          , genCounter = s.genCounter + 1 })

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
                in lifeStep liveCount cell

    lifeStep :: Number -> Cell -> Cell
    lifeStep liveCount cell = case cell of
        Alive -> if liveCount < 2 || liveCount > 3 then Dead else Alive
        Dead  -> if liveCount == 3 then Alive else Dead

    findNeighbours :: Number -> Number -> Generation -> [Cell]
    findNeighbours y x cells = catMaybes maybeNeighbours
        where
        maybeNeighbours = map (\[y, x] -> getByIndex2 cells y x) newCells
        newCells = [ [y-1, x-1], [y-1, x  ], [y-1, x+1],
                     [y,   x-1],             [y,   x+1],
                     [y+1, x-1], [y+1, x  ], [y+1, x+1] ]

calculateNewGeneration :: State -> State
calculateNewGeneration state = saveNewGeneration state newGeneration
  where
  newGeneration = (getCurrentGeneration >>> genNewGeneration) state

updatePoint :: Cell -> State -> Number -> Number -> State
updatePoint newCell state y x = saveNewGeneration state newGeneration
    where currentGeneration = getCurrentGeneration state
          newGeneration = updateAt2 y x newCell currentGeneration

addPoint    = updatePoint Alive
removePoint = updatePoint Dead

togglePoint :: State -> Number -> Number -> State
togglePoint state y x = case getByIndex2 (getCurrentGeneration state) y x of
    Just Alive -> removePoint state y x
    Just Dead  -> addPoint state y x
    _          -> state

toggleTicks :: RunStatus -> Rx.Observable Boolean -> State -> State
toggleTicks rs playPauseStream (State s) = runPure (do
    pure $ onNext playPauseStream $ case rs of
                                        Running -> true
                                        Paused  -> false
    pure $ State (s {runningState = rs}))

play  = toggleTicks Running
pause = toggleTicks Paused

toggle :: Rx.Observable Boolean -> State -> State
toggle playPauseStream state@(State s) | s.runningState == Running = pause playPauseStream state
                                       | s.runningState == Paused  = play playPauseStream state

updateTimer :: forall e. State -> Eff (now :: Now | e) State
updateTimer state@(State s) = do
  n <- now
  pure $ State (s { secondsElapsed = toFixed ((timeDelta s.startTime n) / 1000) 2
                  , genCounter = 0
                  , genRatio = s.genCounter
                  })

-- | This is the application's state machine. It maps `Action`s to new `State`s
updateStateFactory :: Rx.Observable Boolean ->  (forall e. Action -> State -> Eff (now :: Now | e) State)
updateStateFactory playPauseStream = updateState
  where
  updateState Tick              state = pure $ calculateNewGeneration state
  updateState Play              state = pure $ play playPauseStream state
  updateState Pause             state = pure $ pause playPauseStream state
  updateState Toggle            state = pure $ toggle playPauseStream state
  updateState Save              state = pure $ proxyLog state
  updateState (Point y x)       state = pure $ addPoint state y x
  updateState (NoPoint y x)     state = pure $ removePoint state y x
  updateState (TogglePoint y x) state = pure $ togglePoint state y x
  updateState (NewCells cs)     state = pure $ saveNewGeneration state cs
  updateState (Rewind n)        state = pure $ (pause playPauseStream >>> rewind n) state
  updateState (FForward n)      state = pure $ (pause playPauseStream >>> fforward n) state
  updateState Timer             state = updateTimer state
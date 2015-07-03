module Types where

import Data.Date
import Data.Maybe
import Prelude

type Generation = Array (Array Cell)

data State = State { cells          :: Array Generation
                   , runningState   :: RunStatus
                   , current        :: Maybe Int
                   , startTime      :: Date
                   , secondsElapsed :: Number
                   , genCounter     :: Number
                   , genRatio       :: Number
                   }

instance showState :: Show State where
    show (State s) = "State { cells: " ++ show s.cells
                        ++ ", runningState: " ++ show s.runningState
                        ++ ", current: " ++ show s.current
                        ++ ", startTime: " ++ show s.startTime
                        ++ ", genRatio:" ++ show s.genRatio
                        ++ ", genCounter: " ++ show s.genCounter
                        ++ ", secondsElapsed: " ++ show s.secondsElapsed
                        ++ "}"

data Action = Point Int Int
            | NoPoint Int Int
            | TogglePoint Int Int
            | Tick
            | Pause
            | Play
            | Toggle
            | Save
            | NewCells Generation
            | Rewind Int
            | FForward Int
            | Timer
            | RandomGen

instance showAction :: Show Action where
    show (Point y x)        = "Point(" ++ show y ++ ", " ++ show x ++ ")"
    show (NoPoint y x)      = "NoPoint(" ++ show y ++ ", " ++ show x ++ ")"
    show (TogglePoint y x)  = "TogglePoint(" ++ show y ++ ", " ++ show x ++ ")"
    show Tick               = "Tick"
    show Play               = "Play"
    show Pause              = "Pause"
    show Toggle             = "Toggle"
    show Save               = "Save"
    show (NewCells cs)      = "NewCells " ++ show cs
    show (Rewind x)         = "Rewind " ++ show x
    show (FForward x)       = "FForward " ++ show x
    show Timer              = "Timer"
    show RandomGen          = "RandomGen"

data Cell = Alive | Dead

instance eqCell :: Eq Cell where
    eq Alive Alive = true
    eq Dead Dead   = true
    eq _ _         = false

instance showCell :: Show Cell where
    show Alive = "Alive"
    show Dead  = "Dead"

data RunStatus = Running | Paused

instance showRunStatus :: Show RunStatus where
    show Running = "Running"
    show Paused  = "Paused"

instance eqRunStatus :: Eq RunStatus where
    eq Running Running = true
    eq Paused  Paused  = true
    eq _       _       = false

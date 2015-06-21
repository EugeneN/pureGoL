module Types where

import Data.Maybe

type Generation = [[Cell]]

foreign import data Datetime :: *

-- instance showDatetime :: Show Datetime where
    -- show = toString

data State = State { cells        :: [Generation]
                   , runningState :: RunStatus
                   , current      :: Maybe Number
                   , startTime    :: Datetime
                   , secondsElapsed :: Number
                   , genCounter     :: Number
                   , genRatio       :: Number
                   }

instance showState :: Show State where
    show (State s) = "State { cells: " ++ show s.cells
                        ++ ", runningState: " ++ show s.runningState
                        ++ ", current: " ++ show s.current
                        ++ ", startTime: " ++ "show s.startTime"
                        ++ "}"

data Action = Point Number Number
            | NoPoint Number Number
            | Tick
            | Pause
            | Play
            | Toggle
            | Save
            | NewCells Generation
            | Rewind Number
            | FForward Number
            | Timer

instance showAction :: Show Action where
    show (Point y x)   = "Point(" ++ show y ++ ", " ++ show x ++ ")"
    show (NoPoint y x) = "NoPoint(" ++ show y ++ ", " ++ show x ++ ")"
    show Tick          = "Tick"
    show Play          = "Play"
    show Pause         = "Pause"
    show Toggle        = "Toggle"
    show Save          = "Save"
    show (NewCells cs) = "NewCells " ++ show cs
    show (Rewind x)    = "Rewind " ++ show x
    show (FForward x)  = "FForward " ++ show x
    show Timer         = "Timer"

data Cell = Alive | Dead

instance eqCell :: Eq Cell where
    (==) Alive Alive = true
    (==) Dead Dead   = true
    (==) _ _         = false

    (/=) a b         = not $ (==) a b


instance showCell :: Show Cell where
    show Alive = "Alive"
    show Dead  = "Dead"

data RunStatus = Running | Paused

instance showRunStatus :: Show RunStatus where
    show Running = "Running"
    show Paused  = "Paused"

instance eqRunStatus :: Eq RunStatus where
    (==) Running Running = true
    (==) Paused  Paused  = true
    (==) _       _       = false

    (/=) a       b       = not $ (==) a b

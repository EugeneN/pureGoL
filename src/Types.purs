module Types where

data State = State { cells :: [[Cell]]
                   , runningState :: RunStatus }

instance showState :: Show State where
    show (State s) = "State { cells: " ++ show s.cells
                        ++ ", runningState: " ++ show s.runningState ++ "}"

data Action = Point Number Number | NoPoint Number Number | Tick | Pause |  Play | Save

instance showAction :: Show Action where
    show (Point y x)   = "Point(" ++ show y ++ ", " ++ show x ++ ")"
    show (NoPoint y x) = "NoPoint(" ++ show y ++ ", " ++ show x ++ ")"
    show Tick          = "Tick"
    show Play          = "Play"
    show Pause         = "Pause"
    show Save          = "Save"

data Cell = Alive | Dead

instance showCell :: Show Cell where
    show Alive = "Alive"
    show Dead  = "Dead"

data RunStatus = Running | Paused

instance showRunStatus :: Show RunStatus where
    show Running = "Running"
    show Paused  = "Paused"


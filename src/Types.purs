module Types where

data State = State { cells :: [[Cell]]
                   , runningState :: RunStatus }

data Action = Point Number Number | NoPoint Number Number | Interval | Pause |  Play | Dump

data Cell = Alive | Dead

data RunStatus = Running | Paused


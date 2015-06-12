module Types where

data State = State [[Cell]]
data Action = Point Number Number | Interval
data Cell = Alive | Dead


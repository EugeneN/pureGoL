## Module Types

#### `Generation`

``` purescript
type Generation = Array (Array Cell)
```

#### `State`

``` purescript
data State
  = State { cells :: Array Generation, runningState :: RunStatus, current :: Maybe Int, startTime :: Date, secondsElapsed :: Number, genCounter :: Number, genRatio :: Number }
```

##### Instances
``` purescript
instance showState :: Show State
```

#### `Action`

``` purescript
data Action
  = Point Int Int
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
```

##### Instances
``` purescript
instance showAction :: Show Action
```

#### `Cell`

``` purescript
data Cell
  = Alive
  | Dead
```

##### Instances
``` purescript
instance eqCell :: Eq Cell
instance showCell :: Show Cell
```

#### `RunStatus`

``` purescript
data RunStatus
  = Running
  | Paused
```

##### Instances
``` purescript
instance showRunStatus :: Show RunStatus
instance eqRunStatus :: Eq RunStatus
```



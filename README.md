# pureGoL

This is a Game of Life written in Purescript using Rx and React.

See http://eugenen.github.io/pureGoL/


# Module Documentation

## Module Core

#### `getTotalGenerations`

``` purescript
getTotalGenerations :: State -> Number
```


#### `getCurrentGeneration`

``` purescript
getCurrentGeneration :: State -> Generation
```


#### `saveNewGeneration`

``` purescript
saveNewGeneration :: State -> Generation -> State
```


#### `calculateNewGeneration`

``` purescript
calculateNewGeneration :: State -> State
```


#### `updateStateFactory`

``` purescript
updateStateFactory :: Rx.Observable Boolean -> Action -> State -> State
```

This is the application's state machine. It maps `Action`s to new `State`s


## Module Data


## Module KeyCodes

#### `KeyCode`

``` purescript
data KeyCode
  = Insert
  | Escape
  | Enter
  | Delete
  | F1
  | F2
  | F3
  | F4
  | F5
  | Space
  | LeftArrow
  | RightArrow
  | UnknownKey Number
```


#### `keyEventToKeyCode`

``` purescript
keyEventToKeyCode :: forall a. a -> KeyCode
```


#### `eqKeyCode`

``` purescript
instance eqKeyCode :: Eq KeyCode
```


#### `which`

``` purescript
which :: forall a. a -> Number
```



## Module Main


## Module Types

#### `Generation`

``` purescript
type Generation = [[Cell]]
```


#### `Datetime`

``` purescript
data Datetime :: *
```


#### `State`

``` purescript
data State
  = State { genRatio :: Number, genCounter :: Number, secondsElapsed :: Number, startTime :: Datetime, current :: Maybe Number, runningState :: RunStatus, cells :: [Generation] }
```

#### `showState`

``` purescript
instance showState :: Show State
```


#### `Action`

``` purescript
data Action
  = Point Number Number
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
```


#### `showAction`

``` purescript
instance showAction :: Show Action
```


#### `Cell`

``` purescript
data Cell
  = Alive
  | Dead
```


#### `eqCell`

``` purescript
instance eqCell :: Eq Cell
```


#### `showCell`

``` purescript
instance showCell :: Show Cell
```


#### `RunStatus`

``` purescript
data RunStatus
  = Running
  | Paused
```


#### `showRunStatus`

``` purescript
instance showRunStatus :: Show RunStatus
```


#### `eqRunStatus`

``` purescript
instance eqRunStatus :: Eq RunStatus
```



## Module UI

#### `mainView`

``` purescript
mainView :: ComponentClass { state :: State, actionsStream :: Rx.Observable Action } {  }
```


#### `renderMainView`

``` purescript
renderMainView :: forall eff. String -> State -> Rx.Observable Action -> Eff (react :: React, dom :: DOM | eff) Component
```



## Module Utils

#### `proxyLog`

``` purescript
proxyLog :: forall a. a -> a
```


#### `updateAt2`

``` purescript
updateAt2 :: forall a. Number -> Number -> a -> [[a]] -> [[a]]
```


#### `getByIndex2`

``` purescript
getByIndex2 :: forall a. [[a]] -> Number -> Number -> Maybe a
```


#### `now`

``` purescript
now :: forall a. Fn0 Datetime
```


#### `timeDelta`

``` purescript
timeDelta :: Datetime -> Datetime -> Number
```


#### `toFixed`

``` purescript
toFixed :: Number -> Number -> Number
```


#### `newSubject`

``` purescript
newSubject :: forall a. Fn0 (Rx.Observable a)
```


#### `getIntervalStream`

``` purescript
getIntervalStream :: forall a. Number -> Rx.Observable a
```


#### `onNext`

``` purescript
onNext :: forall a. Rx.Observable a -> a -> Rx.Observable a
```


#### `pausable`

``` purescript
pausable :: forall a b. Rx.Observable a -> Rx.Observable b -> Rx.Observable a
```


#### `setProps`

``` purescript
setProps :: forall a eff. Component -> a -> Eff (react :: React, dom :: DOM | eff) Unit
```


#### `fromEvent`

``` purescript
fromEvent :: forall eff z. String -> Rx.Observable z
```





# pureGoL

See http://eugenen.github.io/pureGoL/

# Module Documentation

## Module Core

#### `calculateNewGeneration`

``` purescript
calculateNewGeneration :: State -> State
```

This is the heart of GoL. It calculates a new generation based on
previous one and the rules.

#### `setNewCells`

``` purescript
setNewCells :: State -> [[Cell]] -> State
```


#### `updateState`

``` purescript
updateState :: Rx.Observable Boolean -> Action -> State -> State
```

This is the application's state machine. It maps `Action`s to new `State`s


## Module Data


## Module Main


## Module Types

#### `State`

``` purescript
data State
  = State { runningState :: RunStatus, cells :: [[Cell]] }
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
  | Save 
  | NewCells [[Cell]]
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


#### `newSubject`

``` purescript
newSubject :: forall a. Fn0 (Rx.Observable a)
```


#### `getIntervalStream`

``` purescript
getIntervalStream :: forall a. Number -> Rx.Observable a
```


#### `(~>)`

``` purescript
(~>) :: forall eff a. Rx.Observable a -> (a -> Eff eff Unit) -> Eff eff Unit
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





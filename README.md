# pureGoL

This is a Game of Life written in Purescript using Rx and React/Canvas/Console UIs.

Demo: http://eugenen.github.io/pureGoL/

Console UI video: https://www.youtube.com/watch?v=8G-d8XcT6pM&feature=youtu.be

The main goal of this project is to define a clean and highly decoupled interface
between *stateful* business logic and *stateful* UI. Both business logic and UI
are state machines, black boxes, which communicate using only 2 data types and 2 streams:

```purescript
data State = ...
data Action = ...

type BL2UIChannel = Rx.Observable State
type UI2BLChannel = Rx.Observable Action
```

See http://eugenen.github.io/pureGoL/ for demo.

# Run

- `git clone https://github.com/EugeneN/pureGoL.git`
- `cd pureGoL/`
- `pulp dep install`
- *optional if `browserify` can't find `npm` module for `rx`: `npm install rx`
- `pulp browserify > public/GoL.js`
- `open public/index.html`
# Module Documentation

## Module Core

#### `getInitialState`

``` purescript
getInitialState :: forall e. Eff (now :: Now | e) State
```


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
updateStateFactory :: Rx.Observable Boolean -> (forall e. Action -> State -> Eff (trace :: Trace, now :: Now | e) State)
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


#### `State`

``` purescript
data State
  = State { genRatio :: Number, genCounter :: Number, secondsElapsed :: Number, startTime :: Date, current :: Maybe Number, runningState :: RunStatus, cells :: [Generation] }
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
  | TogglePoint Number Number
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



## Module UI.Canvas

#### `Color`

``` purescript
type Color = String
```


#### `UIEvent`

``` purescript
type UIEvent = String
```

#### `fromUiEvent`

``` purescript
fromUiEvent :: forall a e. e -> UIEvent -> Rx.Observable a
```


#### `setupUI`

``` purescript
setupUI :: forall e. State -> Rx.Observable Action -> String -> Eff (trace :: Trace, dom :: DOM, canvas :: Canvas | e) (Rx.Observable State)
```


#### `renderCanvas`

``` purescript
renderCanvas :: forall e. CanvasElement -> State -> Eff (trace :: Trace, canvas :: Canvas | e) Unit
```


#### `drawBackground`

``` purescript
drawBackground :: forall e. Context2D -> Number -> Number -> Number -> Number -> Eff (canvas :: Canvas | e) Unit
```


#### `drawLabels`

``` purescript
drawLabels :: forall e. Context2D -> State -> Eff (canvas :: Canvas | e) Unit
```


#### `drawBorders`

``` purescript
drawBorders :: forall e. Context2D -> Number -> Number -> Number -> Number -> Eff (canvas :: Canvas | e) Unit
```


#### `drawGrid`

``` purescript
drawGrid :: forall e. Context2D -> Number -> Number -> Number -> Number -> Number -> Number -> Eff (canvas :: Canvas | e) Unit
```


#### `drawCells`

``` purescript
drawCells :: forall e. Context2D -> Generation -> Eff (canvas :: Canvas | e) Unit
```


#### `drawCell`

``` purescript
drawCell :: forall e. Color -> Context2D -> Number -> Number -> Eff (canvas :: Canvas | e) Unit
```


#### `getWidth`

``` purescript
getWidth :: Generation -> Number
```


#### `getHeight`

``` purescript
getHeight :: Generation -> Number
```



## Module UI.Console

#### `exportGlobal`

``` purescript
exportGlobal :: forall e a. String -> (Number -> Number -> Eff e Unit) -> Eff e Unit
```


#### `setupUI`

``` purescript
setupUI :: forall e. State -> Rx.Observable Action -> String -> Eff (trace :: Trace | e) (Rx.Observable State)
```



## Module UI.React

#### `setupUI`

``` purescript
setupUI :: forall e. State -> Rx.Observable Action -> String -> Eff (trace :: Trace, react :: React, dom :: DOM | e) (Rx.Observable State)
```


#### `mainView`

``` purescript
mainView :: ComponentClass { state :: State, actionsStream :: Rx.Observable Action } {  }
```


#### `renderMainView`

``` purescript
renderMainView :: forall eff. String -> State -> Rx.Observable Action -> Eff (react :: React, dom :: DOM | eff) Component
```


#### `setProps`

``` purescript
setProps :: forall a eff. Component -> a -> Eff (react :: React, dom :: DOM | eff) Unit
```



## Module Utils

#### `updateAt2`

``` purescript
updateAt2 :: forall a. Number -> Number -> a -> [[a]] -> [[a]]
```


#### `getByIndex2`

``` purescript
getByIndex2 :: forall a. [[a]] -> Number -> Number -> Maybe a
```


#### `timeDelta`

``` purescript
timeDelta :: Date -> Date -> Number
```


#### `toFixed`

``` purescript
toFixed :: Number -> Number -> Number
```


#### `newSubject`

``` purescript
newSubject :: forall a. Fn0 (Rx.Observable a)
```

#### `fromEvent`

``` purescript
fromEvent :: forall eff z. String -> Eff (dom :: DOM | eff) (Rx.Observable z)
```


#### `getIntervalStream`

``` purescript
getIntervalStream :: forall a. Number -> Rx.Observable a
```


#### `onNext`

``` purescript
onNext :: forall a eff. Rx.Observable a -> a -> Eff eff Unit
```


#### `pausable`

``` purescript
pausable :: forall a b. Rx.Observable a -> Rx.Observable b -> Rx.Observable a
```


#### `scan`

``` purescript
scan :: forall a b e. (a -> b -> Eff e b) -> b -> Rx.Observable a -> Eff e (Rx.Observable b)
```

Modification of the original `purescript-rx` `scan`, which enables
processing function to produce effects

#### `getElementOffsetLeft`

``` purescript
getElementOffsetLeft :: forall e. String -> Eff (dom :: DOM | e) Number
```

#### `getElementOffsetTop`

``` purescript
getElementOffsetTop :: forall e. String -> Eff (dom :: DOM | e) Number
```


#### `getParameterByName`

``` purescript
getParameterByName :: forall e. String -> Eff e String
```

Returns url's query parameters by name

#### `displayBlock`

``` purescript
displayBlock :: forall e. String -> Eff (dom :: DOM | e) Unit
```

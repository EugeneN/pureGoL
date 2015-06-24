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

data BL2UIChannel = Rx.Observable State
data UI2BLChannel = Rx.Observable Action
```

Obviously it should be possible to abstract over Rx.Observable like this:

```purescript
data BL2UIChannel = forall a. Reactive' a => a State
data UI2BLChannel = forall a. Reactive' a => a Action
```

where `Reactive'` means any, well, reactive mechanism. Even more abstracted signature looks like this:

```purescript
data UIComponent m input output = forall e. Reactive' m => input -> m output -> Eff e (m input)
```
where 
- input - type of input state
- output - type of actions sent back to business logic
- m - type of some kind of reactive monad
- e - type of effects performed by the component.
 

A bit similar to purescript-halogen, and most important - it works :-) Even with non-html UIs.
See http://eugenen.github.io/pureGoL/ for demo.


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
setupUI :: forall e. State -> Rx.Observable Action -> String -> Eff (trace :: Trace, canvas :: Canvas | e) (Rx.Observable State)
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



## Module UI.React

#### `mainView`

``` purescript
mainView :: ComponentClass { state :: State, actionsStream :: Rx.Observable Action } {  }
```


#### `renderMainView`

``` purescript
renderMainView :: forall eff. String -> State -> Rx.Observable Action -> Eff (react :: React, dom :: DOM | eff) Component
```


#### `setupUI`

``` purescript
setupUI :: forall e. State -> Rx.Observable Action -> String -> Eff (trace :: Trace, react :: React, dom :: DOM | e) (Rx.Observable State)
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


#### `mathRound`

``` purescript
mathRound :: Number -> Number
```


#### `mathFloor`

``` purescript
mathFloor :: Number -> Number
```


#### `getElementOffsetLeft`

``` purescript
getElementOffsetLeft :: forall a e. a -> Number
```


#### `getElementOffsetTop`

``` purescript
getElementOffsetTop :: forall a e. a -> Number
```


#### `getParameterByName`

``` purescript
getParameterByName :: forall e. String -> Eff e String
```





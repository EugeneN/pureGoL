## Module UI.Canvas

#### `UIEvent`

``` purescript
type UIEvent = String
```

#### `Color`

``` purescript
data Color
  = Color { r :: Int, g :: Int, b :: Int }
```

##### Instances
``` purescript
instance eqColor :: Eq Color
instance ordColor :: Ord Color
instance semiringColor :: Semiring Color
instance ringColor :: Ring Color
instance showColor :: Show Color
```

#### `Direction`

``` purescript
data Direction
  = Up
  | Down
```

#### `LocalState`

``` purescript
data LocalState
  = LocalState { state :: State, color :: Color, dir :: Direction }
```

#### `fromUiEvent`

``` purescript
fromUiEvent :: forall a e. e -> UIEvent -> Observable a
```

#### `setupUI`

``` purescript
setupUI :: forall e. State -> Observable Action -> String -> Eff (canvas :: Canvas, dom :: DOM, console :: CONSOLE, random :: RANDOM, ref :: REF | e) (Observable State)
```

#### `renderCanvas`

``` purescript
renderCanvas :: forall e. CanvasElement -> State -> Color -> Eff (canvas :: Canvas, console :: CONSOLE, random :: RANDOM | e) Unit
```

#### `drawBackground`

``` purescript
drawBackground :: forall e. Context2D -> Int -> Int -> Int -> Int -> Eff (canvas :: Canvas | e) Unit
```

#### `drawLabels`

``` purescript
drawLabels :: forall e. Context2D -> State -> Eff (canvas :: Canvas | e) Unit
```

#### `drawBorders`

``` purescript
drawBorders :: forall e. Context2D -> Int -> Int -> Int -> Int -> Eff (canvas :: Canvas | e) Unit
```

#### `drawGrid`

``` purescript
drawGrid :: forall e. Context2D -> Int -> Int -> Int -> Int -> Int -> Int -> Eff (canvas :: Canvas | e) Unit
```

#### `drawCells`

``` purescript
drawCells :: forall e. Context2D -> Generation -> Color -> Eff (canvas :: Canvas, random :: RANDOM | e) Unit
```

#### `drawSquare`

``` purescript
drawSquare :: forall e. String -> Context2D -> Int -> Int -> Eff (canvas :: Canvas | e) Unit
```

#### `drawCircle`

``` purescript
drawCircle :: forall e. String -> Context2D -> Int -> Int -> Eff (canvas :: Canvas | e) Unit
```

#### `getWidth`

``` purescript
getWidth :: Generation -> Int
```

#### `getHeight`

``` purescript
getHeight :: Generation -> Int
```



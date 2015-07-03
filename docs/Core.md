## Module Core

#### `getInitialState`

``` purescript
getInitialState :: forall e. Eff (now :: Now | e) State
```

#### `getTotalGenerations`

``` purescript
getTotalGenerations :: State -> Int
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

#### `processStateFactory`

``` purescript
processStateFactory :: Observable Boolean -> (forall e. Action -> State -> Eff (now :: Now, console :: CONSOLE, random :: RANDOM | e) State)
```

This is the application's state machine. It maps `Action`s to new `State`s



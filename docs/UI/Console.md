## Module UI.Console

#### `exportGlobal`

``` purescript
exportGlobal :: forall e a. String -> (Int -> Int -> Eff e Unit) -> Eff e Unit
```

#### `setupUI`

``` purescript
setupUI :: forall e. State -> Observable Action -> String -> Eff (console :: CONSOLE | e) (Observable State)
```



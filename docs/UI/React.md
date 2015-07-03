## Module UI.React

#### `setupUI`

``` purescript
setupUI :: forall e. State -> Observable Action -> String -> Eff (dom :: DOM | e) (Observable State)
```

#### `renderMainView`

``` purescript
renderMainView :: forall eff. String -> State -> Observable Action -> Eff (dom :: DOM | eff) UI
```

#### `setProps`

``` purescript
setProps :: forall a eff. UI -> a -> Eff (dom :: DOM | eff) Unit
```



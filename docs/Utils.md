## Module Utils

#### `updateAt2`

``` purescript
updateAt2 :: forall a. Int -> Int -> a -> Array (Array a) -> Array (Array a)
```

#### `getByIndex2`

``` purescript
getByIndex2 :: forall a. Array (Array a) -> Int -> Int -> Maybe a
```

#### `timeDelta`

``` purescript
timeDelta :: Date -> Date -> Number
```

#### `toFixed`

``` purescript
toFixed :: Number -> Number -> Number
```

#### `hex`

``` purescript
hex :: Number -> String
```

#### `newSubject`

``` purescript
newSubject :: forall a. Fn0 (Observable a)
```

#### `fromEvent`

``` purescript
fromEvent :: forall eff z. String -> Eff (dom :: DOM | eff) (Observable z)
```

#### `getIntervalStream`

``` purescript
getIntervalStream :: forall a. Number -> Observable a
```

#### `onNext`

``` purescript
onNext :: forall a eff. Observable a -> a -> Eff eff Unit
```

#### `pausable`

``` purescript
pausable :: forall a b. Observable a -> Observable b -> Observable a
```

#### `scan`

``` purescript
scan :: forall a b e. (a -> b -> Eff e b) -> b -> Observable a -> Eff e (Observable b)
```

Modification of the original `purescript-rx` `scan`, which enables
processing function to produce effects

#### `getElementOffsetLeft`

``` purescript
getElementOffsetLeft :: forall e. String -> Eff (dom :: DOM | e) Int
```

#### `getElementOffsetTop`

``` purescript
getElementOffsetTop :: forall e. String -> Eff (dom :: DOM | e) Int
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

#### `which`

``` purescript
which :: forall a. a -> Int
```



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
  | Rsmall
  | Rbig
  | UnknownKey Int
```

##### Instances
``` purescript
instance eqKeyCode :: Eq KeyCode
```

#### `keyEventToKeyCode`

``` purescript
keyEventToKeyCode :: forall a. a -> KeyCode
```



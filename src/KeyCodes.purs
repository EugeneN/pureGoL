module KeyCodes where

data KeyCode = Insert
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
             | UnknownKey Number

keyEventToKeyCode :: forall a. a -> KeyCode
keyEventToKeyCode x | which x == 13  = Enter
                    | which x == 27  = Escape
                    | which x == 45  = Insert
                    | which x == 46  = Delete
                    | which x == 112 = F1
                    | which x == 113 = F2
                    | which x == 114 = F3
                    | which x == 115 = F4
                    | which x == 116 = F5
                    | which x == 32  = Space
                    | which x == 37  = LeftArrow
                    | which x == 39  = RightArrow
                    | which x == 114 = Rsmall
                    | which x == 82  = Rbig

keyEventToKeyCode x                  = UnknownKey $ which x

instance eqKeyCode :: Eq KeyCode where
    (==) Insert Insert = true
    (==) Escape Escape = true
    (==) Enter  Enter  = true
    (==) Delete Delete = true
    (==) F1     F1     = true
    (==) F2     F2     = true
    (==) F3     F3     = true
    (==) F4     F4     = true
    (==) F5     F5     = true
    (==) Space  Space  = true
    (==) LeftArrow   LeftArrow   = true
    (==) RightArrow  RightArrow  = true
    (==) Rbig        Rbig        = true
    (==) Rsmall      Rsmall      = true
    (==) _      _      = false
    (/=) a      b      = not $ (==) a b


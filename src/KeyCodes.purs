module KeyCodes where

import Utils
import Prelude

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
             | UnknownKey Int

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
    eq Insert Insert = true
    eq Escape Escape = true
    eq Enter  Enter  = true
    eq Delete Delete = true
    eq F1     F1     = true
    eq F2     F2     = true
    eq F3     F3     = true
    eq F4     F4     = true
    eq F5     F5     = true
    eq Space  Space  = true
    eq LeftArrow   LeftArrow   = true
    eq RightArrow  RightArrow  = true
    eq Rbig        Rbig        = true
    eq Rsmall      Rsmall      = true
    eq _      _      = false

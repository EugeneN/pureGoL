module Utils where

import Control.Monad.Eff
import Data.Array
import Data.Function
import Data.Maybe
import Data.Tuple
import DOM (DOM(..))
import qualified Rx.Observable as Rx
import Data.DOM.Simple.Types (HTMLElement(..))
import  Data.Date
import Prelude

import Types

map_ = (<#>)
filter_ = flip filter

updateAt2 :: forall a. Int -> Int -> a -> Array (Array a) -> Array (Array a)
updateAt2 y x newVal arr = map_ (zip arr (0 .. (length arr))) \(Tuple row rowIdx) ->
    if rowIdx == y
        then map_ (zip row (0 .. (length row))) \(Tuple oldVal columnIdx) ->
            if columnIdx == x then newVal else oldVal
        else row

getByIndex2 :: forall a. Array (Array a) -> Int -> Int -> Maybe a
getByIndex2 arr x y = return arr >>= (flip (!!) $ x) >>= (flip (!!) $ y)

foreign import timeDelta :: Date -> Date -> Number

foreign import toFixed :: Number -> Number -> Number

foreign import hex :: Number -> String

-- Rx bindings

foreign import newSubject :: forall a. Fn0 (Rx.Observable a)

foreign import fromEvent :: forall eff z. String -> Eff (dom :: DOM | eff) (Rx.Observable z)

foreign import getIntervalStream :: forall a. Number -> Rx.Observable a

foreign import onNext :: forall a eff. Rx.Observable a -> a -> Eff eff Unit

foreign import pausable :: forall a b. Rx.Observable a -> Rx.Observable b -> Rx.Observable a

-- | Modification of the original `purescript-rx` `scan`, which enables
-- | processing function to produce effects
foreign import scan :: forall a b e. (a -> b -> Eff e b) -> b -> Rx.Observable a -> Eff e (Rx.Observable b)

-- DOM bindings

foreign import getElementOffsetLeft :: forall e. String -> Eff (dom :: DOM | e) Int

foreign import getElementOffsetTop :: forall e. String -> Eff (dom :: DOM | e) Int

-- | Returns url's query parameters by name
foreign import getParameterByName :: forall e. String -> Eff e String

foreign import displayBlock :: forall e. String -> Eff (dom :: DOM | e) Unit

foreign import which :: forall a. a -> Int

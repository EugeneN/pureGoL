module Utils where

import Control.Monad.Eff
import Data.Array
import Data.Function
import Data.Maybe
import Data.Tuple
import DOM (DOM(..))
import React.Types (Component(), React())
import qualified Rx.Observable as Rx

import Types

map_ = flip map
filter_ = flip filter

foreign import proxyLog
    """function proxyLog(a) { console.log(a); return a }
    """ :: forall a. a -> a

updateAt2 :: forall a. Number -> Number -> a -> [[a]] -> [[a]]
updateAt2 y x newVal arr = map_ (zip arr (0 .. (length arr))) \(Tuple row rowIdx) ->
    if rowIdx == y
        then map_ (zip row (0 .. (length row))) \(Tuple oldVal columnIdx) ->
            if columnIdx == x then newVal else oldVal
        else row

getByIndex2 :: forall a. [[a]] -> Number -> Number -> Maybe a
getByIndex2 arr x y = case arr !! x of
    Just row -> case row !! y of
        Just cell -> Just cell
        Nothing   -> Nothing
    Nothing  -> Nothing

foreign import newSubject
    """ var newSubject = function () { return new Rx.Subject() }
    """ :: forall a. Fn0 (Rx.Observable a)

foreign import getIntervalStream
    """ function getIntervalStream(interval) { return Rx.Observable.interval(interval) }
    """ :: forall a. Number -> Rx.Observable a

(~>) :: forall eff a. Rx.Observable a -> (a -> Eff eff Unit) -> Eff eff Unit
(~>) = Rx.subscribe

foreign import onNext
    """ function onNext(obs){ return function (val) { return obs.onNext(val); } }
    """ :: forall a. Rx.Observable a -> a -> Rx.Observable a

foreign import pausable
    """ function pausable(obs){ return function (pauser) { return obs.pausable(pauser); } }
    """ :: forall a b. Rx.Observable a -> Rx.Observable b -> Rx.Observable a

foreign import setProps
    """ function setProps(view) { return function(props) { return function(){ return view.setProps(props); } } }
    """ :: forall a eff. Component
                      -> a
                      -> Eff (dom :: DOM, react :: React | eff) Unit
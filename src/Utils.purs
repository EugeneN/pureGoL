module Utils where

import Control.Monad.Eff
import Data.Array
import Data.Function
import Data.Maybe
import Data.Tuple
import DOM (DOM(..))
import React.Types (Component(), React())
import qualified Rx.Observable as Rx
import Data.DOM.Simple.Types (HTMLElement(..))

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
getByIndex2 arr x y = return arr >>= (flip (!!) $ x) >>= (flip (!!) $ y)

foreign import now
    """var now = function () { return new Date() }
    """ :: forall a. Fn0 Datetime

foreign import timeDelta
    """function timeDelta(a) { return function(b) { return b - a } }
    """ :: Datetime -> Datetime -> Number

foreign import toFixed
    """function toFixed(x) { return function(n) { return x.toFixed(n) } }
    """ :: Number -> Number -> Number

foreign import newSubject
    """ var newSubject = function () { return new Rx.Subject() }
    """ :: forall a. Fn0 (Rx.Observable a)

foreign import getIntervalStream
    """ function getIntervalStream(interval) { return Rx.Observable.interval(interval) }
    """ :: forall a. Number -> Rx.Observable a

foreign import onNext
    """ function onNext(obs){ return function (val) { return obs.onNext(val); } }
    """ :: forall a. Rx.Observable a -> a -> Rx.Observable a

foreign import pausable
    """ function pausable(obs){ return function (pauser) { return obs.pausable(pauser); } }
    """ :: forall a b. Rx.Observable a -> Rx.Observable b -> Rx.Observable a

foreign import setProps
    """ function setProps(view) { return function(props) { return function(){ return view.setProps(props); } } }
    """ :: forall a eff. Component -> a -> Eff (dom :: DOM, react :: React | eff) Unit

foreign import fromEvent
  """function fromEvent(ev) { return Rx.Observable.fromEvent(document.body, ev) }
  """ :: forall eff z. String -> (Rx.Observable z)

foreign import fromElementEvent
  """function fromElementEvent(el) { return function(ev) { return Rx.Observable.fromEvent(el, ev) } }
  """ :: forall eff z. HTMLElement -> String -> (Rx.Observable z)    
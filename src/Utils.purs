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
import  Data.Date

import Types

map_ = flip map
filter_ = flip filter

updateAt2 :: forall a. Number -> Number -> a -> [[a]] -> [[a]]
updateAt2 y x newVal arr = map_ (zip arr (0 .. (length arr))) \(Tuple row rowIdx) ->
    if rowIdx == y
        then map_ (zip row (0 .. (length row))) \(Tuple oldVal columnIdx) ->
            if columnIdx == x then newVal else oldVal
        else row

getByIndex2 :: forall a. [[a]] -> Number -> Number -> Maybe a
getByIndex2 arr x y = return arr >>= (flip (!!) $ x) >>= (flip (!!) $ y)

foreign import timeDelta
    """function timeDelta(a) { return function(b) { return b - a } }
    """ :: Date -> Date -> Number

foreign import toFixed
    """function toFixed(x) { return function(n) { return +x.toFixed(n) } }
    """ :: Number -> Number -> Number

foreign import newSubject
    """ var newSubject = function () { return new Rx.Subject() }
    """ :: forall a. Fn0 (Rx.Observable a)

foreign import getIntervalStream
    """ function getIntervalStream(interval) { return Rx.Observable.interval(interval) }
    """ :: forall a. Number -> Rx.Observable a

foreign import onNext
    """ function onNext(obs){ return function(val) { return function () { return obs.onNext(val); } } }
    """ :: forall a eff. Rx.Observable a -> a -> Eff eff Unit

foreign import pausable
    """ function pausable(obs){ return function (pauser) { return obs.pausable(pauser); } }
    """ :: forall a b. Rx.Observable a -> Rx.Observable b -> Rx.Observable a

foreign import setProps
    """ function setProps(view) { return function(props) { return function(){ return view.setProps(props); } } }
    """ :: forall a eff. Component -> a -> Eff (dom :: DOM, react :: React | eff) Unit

foreign import fromEvent
  """function fromEvent(ev) { return function() {return Rx.Observable.fromEvent(document.body, ev)} }
  """ :: forall eff z. String -> Eff (dom :: DOM | eff) (Rx.Observable z)

foreign import getElementOffsetLeft
    """function getElementOffsetLeft(el){ return function() { return document.getElementById(el).offsetLeft } }
    """ :: forall e. String -> Eff (dom :: DOM | e) Number

foreign import getElementOffsetTop
    """function getElementOffsetTop(el){ return function() { return document.getElementById(el).offsetTop } }
    """ :: forall e. String -> Eff (dom :: DOM | e) Number

foreign import getParameterByName
    """function getParameterByName(name) {
         return function() {
           name = name.replace(/[\[]/, "\\[").replace(/[\]]/, "\\]");
           var regex = new RegExp("[\\?&]" + name + "=([^&#]*)"),
               results = regex.exec(location.search);
           return results === null ? "" : decodeURIComponent(results[1].replace(/\+/g, " "));
         }
       }
    """ :: forall e. String -> Eff e String

foreign import displayBlock
    """function displayBlock(elid) {return function() {document.getElementById(elid).style.display = "block"} }
    """ :: forall e. String -> Eff (dom :: DOM | e) Unit

foreign import scan
  """
  function scan(f) {
    return function(seed) {
      return function(ob) {
        return function() {
          return ob.scan(seed, function(acc, value) {
            return f(value)(acc)();
          });
        };
      };
    };
  }
  """ :: forall a b e. (a -> b -> Eff e b) -> b -> Rx.Observable a -> Eff e (Rx.Observable b)

module Utils where

import Control.Monad.Eff (Eff(..))
import DOM (DOM(..))
import React ( createClass , eventHandler , renderComponentById , spec )
import React.Types ( Component() , ComponentClass() , Event() , React()
                   , ReactFormEvent() , ReactThis() )
import qualified Rx.Observable as Rx
import Types
import Data.Array
import Data.Maybe

map_ = flip map
filter_ = flip filter

foreign import updateArray2
    """
    function updateArray2(arr) {
        return function (x) {
            return function (y) {
                return function (val) {
                    return function () {
                        arr[x][y] = val; // make new copy
                    }
                }
            }
        }
    }
    """ :: forall a. [[a]] -> Number -> Number -> a -> [[a]]


getByIndex2 :: forall a. [[a]] -> Number -> Number -> Maybe a
getByIndex2 arr x y = case arr !! x of
    Just row -> case row !! y of
        Just cell -> Just cell
        Nothing   -> Nothing
    Nothing  -> Nothing

foreign import getNewObservable
    """
    function getNewObservable(x) {
        return new Rx.Subject()
    }
    """ :: forall a b. a -> Rx.Observable b

foreign import callEventHandler
    """
    function callEventHandler(f){
        return function(e){ return f(e)() }
    }
    """ :: forall f e eff. f -> e -> Eff ( | eff) Unit

foreign import getIntervalStream
    """
    function getIntervalStream(interval) {
        //return function() {
            return Rx.Observable.interval(interval)
        //}
    }
    """ :: forall a. Number -> Rx.Observable a

{-
foreign import getIntervalStream
    """
      function getIntervalStream(n) {
        return Rx.Observable.interval(n);
      }
    """ :: forall a. a -> Rx.Observable a
-}

(~>) :: forall eff a. Rx.Observable a -> (a -> Eff eff Unit) -> Eff eff Unit
(~>) = Rx.subscribe

foreign import publishToObservable
    """
    function publishToObservable(obs){
        return function (val) {
            //return function() {
                console.log("publishing to observable", obs, val);
                return obs.onNext(val);
            //}
        }
    }
    """ :: forall a. Rx.Observable a -> a -> Rx.Observable a


foreign import setProps
    """
    function setProps(view) {
        return function(props) {
            return function(){
                view.setProps(props)
                return {}
            }
        }
    }
    """ :: forall a eff. a
                      -> {cells :: [[Cell]], pointsStream :: Rx.Observable Action }
                      ->  Eff (dom :: DOM, react :: React | eff) Unit
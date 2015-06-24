module UI.Console where

import Core
import Types
import Utils

import Control.Monad.Eff (Eff(..))
import Debug.Trace
import Data.Array
import Data.Function
import Data.Tuple
import Data.Foldable
import qualified Rx.Observable as Rx

foreign import exportGlobal
  """function exportGlobal(fname) {return function(f) {return function() {window[fname] = f; return {}; } } }
  """ :: forall e a. String -> (Number -> Number -> Eff e Unit) -> Eff e Unit


setupUI :: forall e. State -> Rx.Observable Action -> String -> Eff ( trace :: Trace | e) (Rx.Observable State)
setupUI state actionsStream _ = do
    let vStream = runFn0 newSubject

    exportGlobal "point"    $ \x y -> void $ pure $ onNext actionsStream $ Point x y
    exportGlobal "noPoint"  $ \x y -> void $ pure $ onNext actionsStream $ NoPoint x y
    exportGlobal "play"     $ \_ _ -> void $ pure $ onNext actionsStream Play
    exportGlobal "pause"    $ \_ _ -> void $ pure $ onNext actionsStream Pause

    vStream `Rx.subscribe` printCells
    pure vStream

    where

    printCells state = trace $ charCells
        where
        currentGeneration   = getCurrentGeneration state
        charCells = foldRows currentGeneration

        foldRows = foldl (\acc row -> acc ++ "\n" ++ (foldRow row)) ""
        foldRow  = foldl (\acc cell -> acc ++ (toChar cell)) ""

        toChar Alive = "x"
        toChar Dead  = "."

module UI.Console where

import Core
import Types
import Utils

import Control.Monad.Eff (Eff(..))
import Control.Monad.Eff.Console
import Data.Array
import Data.Function
import Data.Tuple
import Data.Foldable
import qualified Rx.Observable as Rx
import Prelude

foreign import exportGlobal :: forall e a. String -> (Int -> Int -> Eff e Unit) -> Eff e Unit


setupUI :: forall e. State -> Rx.Observable Action -> String -> Eff (console :: CONSOLE | e) (Rx.Observable State)
setupUI state actionsStream _ = do
    let vStream = runFn0 newSubject

    exportGlobal "point"    $ \x y -> onNext actionsStream $ Point x y
    exportGlobal "noPoint"  $ \x y -> onNext actionsStream $ NoPoint x y
    exportGlobal "play"     $ \_ _ -> onNext actionsStream $ Play
    exportGlobal "pause"    $ \_ _ -> onNext actionsStream $ Pause

    vStream `Rx.subscribe` printCells
    pure vStream

    where

    printCells state = log charCells
        where
        currentGeneration = getCurrentGeneration state
        charCells = foldRows currentGeneration

        foldRows = foldl (\acc row -> acc ++ "\n" ++ (foldRow row)) ""
        foldRow  = foldl (\acc cell -> acc ++ (toChar cell)) ""

        toChar Alive = "x"
        toChar Dead  = "."

module Main where

import Debug.Trace
import Types
import Utils
import UI

import Data.Maybe
import Data.Tuple
import Data.Array
import Control.Monad.Eff (Eff(..))
import DOM (DOM(..))
import React ( createClass , eventHandler , renderComponentById , spec )
import React.Types ( Component() , ComponentClass() , Event() , React()
                   , ReactFormEvent() , ReactThis() )
import qualified Rx.Observable as Rx


initialCells = [
 [Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead],
 [Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Alive,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead],
 [Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Alive,Dead,Alive,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead],
 [Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Alive,Alive,Dead,Dead,Dead,Dead,Dead,Dead,Alive,Alive,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Alive,Alive,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead],
 [Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Alive,Dead,Dead,Dead,Alive,Dead,Dead,Dead,Dead,Alive,Alive,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Alive,Alive,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead],
 [Dead,Alive,Alive,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Alive,Dead,Dead,Dead,Dead,Dead,Alive,Dead,Dead,Dead,Alive,Alive,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead],
 [Dead,Alive,Alive,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Alive,Dead,Dead,Dead,Alive,Dead,Alive,Alive,Dead,Dead,Dead,Dead,Alive,Dead,Alive,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead],
 [Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Alive,Dead,Dead,Dead,Dead,Dead,Alive,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Alive,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead],
 [Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Alive,Dead,Dead,Dead,Alive,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead],
 [Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Alive,Alive,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead],
 [Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead],
 [Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead],
 [Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead],
 [Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead],
 [Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead],
 [Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead],
 [Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead],
 [Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead],
 [Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead],
 [Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead],
 [Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead],
 [Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead],
 [Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead],
 [Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead],
 [Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead],
 [Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead],
 [Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead],
 [Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead],
 [Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead],
 [Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead],
 [Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead],
 [Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead],
 [Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead],
 [Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead],
 [Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead],
 [Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead],
 [Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead],
 [Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead]
 ]

{-
initialCells = [ [Dead,  Dead,  Dead,  Dead,  Dead ]
               , [Dead,  Dead,  Dead,  Dead,  Dead ]
               , [Dead,  Alive, Alive, Dead,  Dead ]
               , [Dead,  Alive, Alive, Dead,  Alive]
               , [Dead,  Dead,  Dead,  Dead,  Dead ] ]
-}
initialState = State initialCells



calculateNewGeneration :: State -> State
calculateNewGeneration (State cells) = State $
    map_ (zip cells (0 .. (length cells))) \(Tuple row rowIdx) ->
        map_ (zip row (0 .. (length row))) \(Tuple cell cellIdx) ->
            let neighbours = findNeighbours rowIdx cellIdx cells
                liveCount = length (filter isAlive neighbours)
            in case cell of
                Alive -> if liveCount < 2 || liveCount > 3 then Dead else Alive
                Dead  -> if liveCount == 3 then Alive else Dead

  where
  isAlive :: Cell -> Boolean
  isAlive Alive = true
  isAlive Dead  = false

  justNeighbour :: Maybe Cell -> Boolean
  justNeighbour Nothing  = false
  justNeighbour _        = true

  unpackNeighbours :: Maybe Cell -> Cell
  unpackNeighbours (Just x) = x

  findNeighbours :: Number -> Number -> [[Cell]] -> [Cell]
  findNeighbours y x cells =
    let newCells = [ [y-1, x-1], [y,   x-1], [y+1, x-1], [y-1, x]
                   , [y+1, x],   [y-1, x+1], [y,   x+1], [y+1, x+1] ]

        maybeNeighbours = map (\[y, x] -> getByIndex2 cells y x) newCells
        allNeighbours = filter justNeighbour maybeNeighbours


    in
        map unpackNeighbours allNeighbours

addPoint :: State -> Action -> State
addPoint (State cells) (Point y x) = State $ updateArray2 cells y x Alive
addPoint s _ = s -- should be error probably

main = do
    view <- renderMainView "root_layout" initialState pointsStream
    scanStream ~> \(State cells) -> setProps view {cells: cells, pointsStream: pointsStream}


  where
  intervalStream = (\_ -> Interval) <$> (getIntervalStream 500)
  pointsStream = getNewObservable 1
  actionsStream = intervalStream <> pointsStream
  scanStream = Rx.scan updateState initialState actionsStream

  updateState :: Action -> State -> State
  updateState Interval      state = calculateNewGeneration state
  updateState p@(Point _ _) state = addPoint state p



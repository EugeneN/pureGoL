module UI where

import Types
import Utils
import Debug.Trace

import Control.Monad.Eff (Eff(..))
import DOM (DOM(..))
import qualified React.DOM as D
import React ( createClass , eventHandler , renderComponentById , spec )
import React.Types ( Component() , ComponentClass() , Event() , React()
                   , ReactFormEvent() , ReactThis() )
import qualified Rx.Observable as Rx
import Data.Tuple
import Data.Array


mainView :: ComponentClass { pointsStream :: Rx.Observable Action, cells :: [[Cell]] } {}
mainView = createClass spec { displayName = "MainView", render = renderFun } where
    renderFun this = pure $
        D.div { className: "user-info"} [
            D.table { style: { border: "1px solid gray" } } [
                D.tbody {} $
                    map_ (zip this.props.cells (0 .. (length this.props.cells))) \(Tuple row rowIdx) ->
                        D.tr {} $
                            map_ (zip row (0 .. (length row))) \(Tuple cell cellIdx) ->
                                case cell of
                                    Alive -> D.td { className: "live" } []
                                    Dead  -> D.td { className: "dead"
                                                  , onClick: \_ -> publishToObservable this.props.pointsStream (Point rowIdx cellIdx)
                                                  } []


            ]
        ]

renderMainView :: forall eff. String
                           -> State
                           -> Rx.Observable Action
                           -> Eff (dom :: DOM, react :: React | eff) Component
renderMainView targetId (State cells) pointsStream =
    renderComponentById (mainView { pointsStream: pointsStream, cells: cells } []) targetId
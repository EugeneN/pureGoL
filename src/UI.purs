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


mainView :: ComponentClass { pointsStream :: Rx.Observable Action, state :: State } {}
mainView = createClass spec { displayName = "MainView", render = renderFun } where
    renderFun this = render this.props.pointsStream this.props.state

    render pointsStream (State { cells = cells, runningState = runningState }) = pure $
        D.div { className: "map"} [
            case runningState of
                Running -> D.button { onClick: \_ -> onNext pointsStream Pause } [D.rawText "Pause" ]
                Paused  -> D.button { onClick: \_ -> onNext pointsStream Play  } [D.rawText "Play" ]

          , D.button { onClick: \_ -> onNext pointsStream Dump } [D.rawText "Dump" ]

          , D.table { style: { border: "1px solid gray" } } [
                D.tbody {} $
                    map_ (zip cells (0 .. (length cells))) \(Tuple row rowIdx) ->
                        D.tr {} $
                            map_ (zip row (0 .. (length row))) \(Tuple cell cellIdx) ->
                                case cell of
                                    Alive -> D.td { className: "live" } []
                                    Dead  -> D.td { className: "dead"
                                                  , onClick: \_ -> onNext pointsStream (Point rowIdx cellIdx)
                                                  } []
            ]
        ]

renderMainView :: forall eff. String
                           -> State
                           -> Rx.Observable Action
                           -> Eff (dom :: DOM, react :: React | eff) Component
renderMainView targetId state pointsStream =
    renderComponentById (mainView { pointsStream: pointsStream, state: state } []) targetId
module UI where

import Data
import Types
import Utils

import Control.Monad.Eff (Eff(..))
import Data.Array
import Data.Tuple
import Debug.Trace
import DOM (DOM(..))
import qualified React.DOM as D
import React (createClass, eventHandler, renderComponentById, spec)
import React.Types (Component(), ComponentClass(), Event(), React())
import qualified Rx.Observable as Rx


mainView :: ComponentClass { actionsStream :: Rx.Observable Action, state :: State } {}
mainView = createClass spec { displayName = "MainView", render = renderFun } where
    renderFun this = render this.props.actionsStream this.props.state

    render actionsStream (State { cells = cells, runningState = runningState }) = pure $
        D.div { className: "map"} [
            case runningState of
                Running -> D.button { onClick: \_ -> onNext actionsStream Pause } [D.rawText "Pause"]
                Paused  -> D.button { onClick: \_ -> onNext actionsStream Play  } [D.rawText "Play" ]

          , D.button { onClick: \_ -> onNext actionsStream Save } [D.rawText "Save"]

          --, D.button { onClick: \_ -> onNext actionsStream (NewCells initialCells)  } [D.rawText "Cells 1"]
          --, D.button { onClick: \_ -> onNext actionsStream (NewCells initialCells2) } [D.rawText "Cells 2"]

          , D.table { style: { border: "1px solid gray" } } [
                D.tbody {} $
                    map_ (zip cells (0 .. (length cells))) \(Tuple row rowIdx) ->
                        D.tr {} $
                            map_ (zip row (0 .. (length row))) \(Tuple cell cellIdx) ->
                                case cell of
                                    Alive -> D.td { className: "live"
                                                  , onClick: \_ -> onNext actionsStream (NoPoint rowIdx cellIdx)
                                                  } []
                                    Dead  -> D.td { className: "dead"
                                                  , onClick: \_ -> onNext actionsStream (Point rowIdx cellIdx)
                                                  } []
            ]
        ]

renderMainView :: forall eff. String
                           -> State
                           -> Rx.Observable Action
                           -> Eff (dom :: DOM, react :: React | eff) Component
renderMainView targetId state actionsStream =
    renderComponentById (mainView { actionsStream: actionsStream, state: state } []) targetId
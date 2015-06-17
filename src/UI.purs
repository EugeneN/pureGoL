module UI where

import Data
import Types
import Utils
import Core

import Control.Monad.Eff (Eff(..))
import Data.Array
import Data.Function
import Data.Maybe
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



    render actionsStream state@(State s) =
        let currentGeneration = getCurrentGeneration state
            totalGenerations = getTotalGenerations state
            timeElapsed = toFixed ((timeDelta s.startTime (runFn0 now)) / 1000) 2
            genSec = toFixed (totalGenerations / timeElapsed) 2
        in pure $
            D.div { className: "map"} [
                D.div { className: "toolbar" } [
                    case s.runningState of
                        Running -> D.button { className: "icon-button", onClick: \_ -> onNext actionsStream Pause } [D.rawText "▮▮"]
                        Paused  -> D.button { className: "icon-button", onClick: \_ -> onNext actionsStream Play  } [D.rawText "▶" ]

                  , D.button { onClick: \_ -> onNext actionsStream Save } [D.rawText "Save"]

                  , D.span { className: "label" } [D.rawText $ "Time elapsed, s: " ++ show timeElapsed ]
                  , D.span { className: "label" } [D.rawText $ "Gen/sec: " ++ show genSec ]
                ]
              , D.div { className: "toolbar" } [
                    D.button { className: "icon-button", onClick: \_ -> onNext actionsStream (Rewind 1)   } [D.rawText "◀◀"]
                  , D.button { className: "icon-button", onClick: \_ -> onNext actionsStream (FForward 1) } [D.rawText "▶▶"]

                  , D.span { className: "label" } [D.rawText $ "Current generation: " ++ case s.current of
                                                                         Nothing -> "Latest"
                                                                         Just x -> show x ]

                  , D.span { className: "label" } [D.rawText $ "Total generations: " ++ show totalGenerations  ]
              ]

              , D.table { style: { border: "1px solid gray", "margin-top": "10px" } } [
                    D.tbody {} $
                        map_ (zip currentGeneration (0 .. (length currentGeneration))) \(Tuple row rowIdx) ->
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
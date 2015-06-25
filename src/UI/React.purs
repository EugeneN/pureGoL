module UI.React where

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


setupUI :: forall e. State -> Rx.Observable Action -> String
                  -> Eff (dom :: DOM, react :: React, trace :: Trace | e) (Rx.Observable State)
setupUI initialState actionsStream targetId = do
    displayBlock targetId
    view <- renderMainView targetId initialState actionsStream

    let vStream = runFn0 newSubject
    vStream `Rx.subscribe` (\s -> setProps view { actionsStream: actionsStream, state: s })

    pure vStream

mainView :: ComponentClass { actionsStream :: Rx.Observable Action, state :: State } {}
mainView = createClass spec { displayName = "MainView", render = renderFun } where
    renderFun this = render this.props.actionsStream this.props.state

    render actionsStream state@(State s) =
        let currentGeneration = getCurrentGeneration state
            totalGenerations = getTotalGenerations state
        in pure $
            D.div { className: "map"} [
                D.div { className: "toolbar" } [
                    case s.runningState of
                        Running -> D.button { className: "icon-button", onClick: \_ -> onNext actionsStream Pause } [D.rawText "▮▮"]
                        Paused  -> D.button { className: "icon-button", onClick: \_ -> onNext actionsStream Play  } [D.rawText "▶" ]

                  , D.button { onClick: \_ -> onNext actionsStream Save } [D.rawText "Save"]

                  , D.span { className: "label" } [D.rawText $ "Time elapsed, s: " ++ show s.secondsElapsed ]
                  , D.span { className: "label" } [D.rawText $ "Gen/sec: " ++ show s.genRatio ]
                ]
              , D.div { className: "toolbar" } [
                    D.button { className: "icon-button", onClick: \_ -> onNext actionsStream (Rewind 1)   } [D.rawText "◀◀"]
                  , D.button { className: "icon-button", onClick: \_ -> onNext actionsStream (FForward 1) } [D.rawText "▶▶"]

                  , D.span { className: "label" } [D.rawText $ "Current generation: " ++ case s.current of
                                                                         Nothing -> "Latest"
                                                                         Just x -> show x ]

                  , D.span { className: "label" } [D.rawText $ "Total generations: " ++ show totalGenerations  ]
              ]

              , D.div { className: "toolbar" } [
                    D.span { style: {"fontSize": "80%"} } [ D.span {className: "button-like"} [D.rawText "Space"]
                                                          , D.rawText " - toggle play/pause, "
                                                          , D.span {className: "button-like"} [D.rawText "⟵"]
                                                          , D.span {className: "button-like"} [D.rawText "⟶"]
                                                          , D.rawText " - navigate generations. Or use buttons above."
                                                          ]
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


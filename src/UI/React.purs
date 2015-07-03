module UI.React where

import Data
import Types
import Utils
import Core

import Control.Monad.Eff (Eff(..))
import Data.Array hiding (span, head)

import Data.Maybe
import Data.Function
import Data.Tuple
import DOM (DOM(..))
import React
import React.DOM
import qualified React.DOM.Props as P
import qualified Rx.Observable as Rx
import Prelude hiding (sub, map, div)


setupUI :: forall e. State -> Rx.Observable Action -> String
                  -> Eff (dom :: DOM | e) (Rx.Observable State)
setupUI initialState actionsStream targetId = do
    displayBlock targetId
    view <- renderMainView targetId initialState actionsStream

    let vStream = runFn0 newSubject
    vStream `Rx.subscribe` (\s -> setProps view { actionsStream: actionsStream, state: s })

    pure vStream

renderMainView :: forall eff. String
                           -> State
                           -> Rx.Observable Action
                           -> Eff (dom :: DOM | eff) UI
renderMainView targetId state actionsStream =
    renderToElementById targetId $ mainView { actionsStream: actionsStream, state: state }

mainView = mkUI $ spec {} aRender

aRender this = do
    p@{state = (State s)} <- getProps this

    let currentGeneration = getCurrentGeneration p.state
        totalGenerations = getTotalGenerations p.state
        actionsStream = p.actionsStream

    pure $
        div [ P.className "map" ] [
            div [ P.className "toolbar" ] [
                case s.runningState of
                    Running -> button [ P.className "icon-button", P.onClick (\_ -> onNext actionsStream Pause) ] [text "▮▮"]
                    Paused  -> button [ P.className "icon-button", P.onClick (\_ -> onNext actionsStream Play)  ] [text "▶" ]

                  , button [ P.onClick (\_ -> onNext actionsStream Save) ] [text "Save"]

                  , span [ P.className "label" ] [text $ "Time elapsed, s: " ++ show s.secondsElapsed ]
                  , span [ P.className "label" ] [text $ "Gen/sec: " ++ show s.genRatio ]
            ]

          , div [ P.className "toolbar" ] [
                button [ P.className "icon-button", P.onClick (\_ -> onNext actionsStream (Rewind 1))   ] [text "◀◀"]
              , button [ P.className "icon-button", P.onClick (\_ -> onNext actionsStream (FForward 1)) ] [text "▶▶"]

              , span [ P.className "label" ] [text $ "Current generation: " ++ case s.current of
                                                                                Nothing -> "Latest"
                                                                                Just x -> show x ]

              , span [ P.className "label" ] [text $ "Total generations: " ++ show totalGenerations  ]
            ]

          , div [ P.className "toolbar" ] [
                span [ P.style {"fontSize": "80%"} ] [ span [P.className "button-like"] [text "Space"]
                                                    , text " - toggle play/pause, "
                                                    , span [P.className "button-like"] [text "⟵"]
                                                    , span [P.className "button-like"] [text "⟶"]
                                                    , text " - navigate generations,"
                                                    , span [P.className "button-like"] [text "r"]
                                                    , text " - random generation. Or use buttons above."
                                                    ]
            ]

          , table [ P.style { border: "1px solid gray", "margin-top": "10px" } ] [
                tbody [] $
                    map_ (zip currentGeneration (0 .. (length currentGeneration))) \(Tuple row rowIdx) ->
                        tr [] $
                            map_ (zip row (0 .. (length row))) \(Tuple cell cellIdx) ->
                                case cell of
                                    Alive -> td [ P.className "live"
                                                , P.onClick (\_ -> onNext actionsStream (NoPoint rowIdx cellIdx))
                                                ] []
                                    Dead  -> td [ P.className "dead"
                                                , P.onClick (\_ -> onNext actionsStream (Point rowIdx cellIdx))
                                                ] []
            ]
        ]



foreign import setProps :: forall a eff. UI -> a -> Eff (dom :: DOM | eff) Unit

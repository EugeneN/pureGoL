module UI.Canvas where

import Data
import Types
import Utils
import Core

import Control.Monad.Eff (Eff(..))
import Data.Array

import Graphics.Canvas

import Control.Monad
import Control.Apply
import Data.Traversable

import Data.Function
import Data.Maybe
import Data.Tuple
import Debug.Trace
import DOM (DOM(..))

import qualified Rx.Observable as Rx

import Data.DOM.Simple.Types (HTMLElement(..))
import Data.DOM.Simple.Window
import Data.DOM.Simple.Events

type Color = String

-- data UIEvent = Click | RClick
type UIEvent = String

cellSize    = 10
topOffset   = 90
leftOffset  = 0

white       = "#ffffff"
black       = "#000000"

bgColor     = white
borderColor = black
gridColor   = "#F8F8F8"
cellColor   = black
labelColor  = black

foreign import fromUiEvent
  """function fromUiEvent(el) {return function(ev) {return Rx.Observable.fromEvent(el, ev) } }
  """ :: forall a e. e -> UIEvent -> Rx.Observable a

setupUI :: forall e. State -> Rx.Observable Action -> String -> Eff (canvas :: Canvas, trace :: Trace | e) (Rx.Observable State)
setupUI state actionsStream canvasId = do
    Just canvas <- getCanvasElementById canvasId

    let rawClicksStream = fromUiEvent canvas "click"
        pxStream = eventToCoords <$> rawClicksStream
        fieldStream = coordsInField `Rx.filter` pxStream
        cellsClicksStream = pxToCell <$> fieldStream
        vStream = runFn0 newSubject

    cellsClicksStream `Rx.subscribe` postUpstream
    vStream `Rx.subscribe` (renderCanvas canvas)

    pure vStream

    where
    postUpstream (Tuple x y) = void $ pure $ onNext actionsStream (TogglePoint y x)

    currentGeneration   = getCurrentGeneration state
    width               = getWidth currentGeneration
    height              = getHeight currentGeneration
    fieldWidth          = width * cellSize
    fieldHeight         = height * cellSize

    fieldOffsetTop = topOffset + (getElementOffsetTop "canvas")
    fieldOffsetLeft = leftOffset + (getElementOffsetLeft "canvas")

    eventToCoords e = Tuple e.pageX e.pageY

    coordsInField (Tuple x y) = x > fieldOffsetLeft
                             && x < fieldOffsetLeft + fieldWidth 
                             && y > fieldOffsetTop
                             && y < fieldOffsetTop + fieldHeight -- XXX FIXME

    pxToCell (Tuple x y) = Tuple (mathFloor $ (x - fieldOffsetLeft) / cellSize)
                                 (mathFloor $ (y - fieldOffsetTop) / cellSize)


renderCanvas :: forall e. CanvasElement -> State -> Eff (canvas :: Canvas, trace :: Trace | e) Unit
renderCanvas canvas state@(State s) = do
    ctx <- getContext2D canvas

    drawBackground  ctx 0 0 maxX maxY
    drawGrid        ctx width height minX minY maxX maxY
    drawBorders     ctx minX minY maxX maxY
    drawCells       ctx currentGeneration
    drawLabels      ctx state

    return unit

    where
    width               = getWidth currentGeneration
    height              = getHeight currentGeneration
    widthPx             = width * cellSize
    heightPx            = height * cellSize
    minX                = leftOffset
    minY                = topOffset
    maxX                = widthPx + leftOffset
    maxY                = heightPx + topOffset

    currentGeneration   = getCurrentGeneration state
    totalGenerations    = getTotalGenerations state


drawBackground :: forall e. Context2D
                         -> Number -> Number -> Number -> Number
                         -> Eff (canvas :: Canvas | e) Unit
drawBackground ctx minX minY maxX maxY = do
    save ctx

    setFillStyle bgColor ctx
    fillPath ctx $ rect ctx { x: minX, y: minY, w: maxX, h: maxY }

    restore ctx
    return unit

drawLabels :: forall e. Context2D -> State ->  Eff (canvas :: Canvas | e) Unit
drawLabels ctx state@(State s) = do
    save ctx
    setFillStyle labelColor ctx
    setFont "16px Source Code Pro" ctx

    fillText ctx (show s.runningState) 5 20
    fillText ctx ("Time elapsed, s: " ++ show s.secondsElapsed) 5 40
    fillText ctx ("Gen/sec: " ++ show s.genRatio) 350 40

    fillText ctx ("Current generation: " ++ (getCurrentGenerationLabel s.current)) 5 60
    fillText ctx ("Total generations: " ++ show (getTotalGenerations state)) 350 60

    setFont "12px Source Code Pro" ctx
    fillText ctx ("Space - toggle play/pause, ⟵⟶ - navigate generations.") 5 80

    restore ctx
    return unit

    where
    getCurrentGenerationLabel x = case x of
        Nothing -> "Latest"
        Just x -> show x

drawBorders :: forall e. Context2D
                      -> Number -> Number -> Number -> Number
                      ->  Eff (canvas :: Canvas | e) Unit
drawBorders ctx minX minY maxX maxY = do
    save ctx

    setLineWidth 1 ctx
    setStrokeStyle borderColor ctx
    beginPath ctx

    moveTo ctx minX minY
    lineTo ctx (maxX + cellSize) minY
    lineTo ctx (maxX + cellSize) maxY
    lineTo ctx minX maxY
    lineTo ctx minX minY

    stroke ctx

    restore ctx
    return unit

drawGrid :: forall e. Context2D
                   -> Number -> Number -> Number -> Number ->  Number -> Number
                   -> Eff (canvas :: Canvas | e) Unit
drawGrid ctx x y minX minY maxX maxY = do
    save ctx

    setLineWidth 1 ctx
    setStrokeStyle gridColor ctx
    beginPath ctx

    sequence $ map_ (0 .. y) \y' -> do
        moveTo ctx minX ((y' * cellSize) + topOffset)
        lineTo ctx maxX ((y' * cellSize) + topOffset)

    sequence $ map_ (0 .. x) \x' -> do
        moveTo ctx ((x' * cellSize) + leftOffset) minY
        lineTo ctx ((x' * cellSize) + leftOffset) maxY

    stroke ctx

    restore ctx
    return unit

drawCells :: forall e. Context2D -> Generation -> Eff (canvas :: Canvas | e) Unit
drawCells ctx cells = do
    save ctx

    for (zip cells (0 .. (length cells))) $ \(Tuple row y) ->
        for (zip row (0 .. (length row))) $ \(Tuple cell x) ->
            case cell of
                Alive -> drawCell cellColor ctx x y
                Dead -> pure unit

    restore ctx
    return unit

drawCell :: forall e. Color -> Context2D -> Number -> Number -> Eff (canvas :: Canvas | e) Unit
drawCell color ctx x y = do
    save ctx

    setFillStyle color ctx
    fillPath ctx $ rect ctx { x: ((x * cellSize) + 1 + leftOffset)
                            , y: ((y * cellSize) + 1 + topOffset)
                            , w: (cellSize - 1)
                            , h: (cellSize - 1) }

    restore ctx
    return unit

getWidth :: Generation -> Number
getWidth (y:ys) = length y

getHeight :: Generation -> Number
getHeight xs = length xs


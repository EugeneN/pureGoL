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
import Control.Monad.Eff.Random

import Data.Function
import Data.Maybe
import Data.Tuple
import DOM (DOM(..))
import Control.Monad.Eff.Ref
import Control.Monad.Eff.Console

import qualified Rx.Observable as Rx

import Data.DOM.Simple.Types (HTMLElement(..))
import Data.DOM.Simple.Window
import Data.DOM.Simple.Events
import Prelude
import Data.Int

import qualified Math as M

-- type Color = String

-- data UIEvent = Click | RClick
type UIEvent = String

cellSize    = 10
halfCell    = 5
topOffset   = 90
leftOffset  = 0

white       = "#ffffff"
black       = "#000000"

bgColor     = white
borderColor = black
gridColor   = "#F8F8F8"
cellColor   = black
labelColor  = black

data Color = Color { r :: Int, g :: Int, b :: Int }

instance eqColor :: Eq Color where
    eq (Color a) (Color b) = a.r == b.r && a.g == b.g && a.b == b.b

instance ordColor :: Ord Color where
    compare c@(Color a) c'@(Color b) =
        if c == c'
        then EQ
        else if a.r < b.r && a.g < b.g && a.b < b.b
            then LT
            else GT

instance semiringColor :: Semiring Color where
    add (Color {r=r, g=g, b=b})
        (Color {r=r',g=g',b=b'}) = Color { r: r + r'
                                         , g: g + g'
                                         , b: b + b' }

    zero                    = Color { r: 0, g: 0, b: 0 }

    mul (Color a) (Color b) = Color { r: a.r * b.r
                                    , g: a.g * b.g
                                    , b: a.b * b.b }
    one                     = Color { r: 1, g: 1, b: 1 }

instance ringColor :: Ring Color where
    sub (Color a) (Color b) = Color { r: (a.r - b.r), g: (a.g - b.g), b: (a.b - b.b) }

instance showColor :: Show Color where
    show (Color a) = "#" ++ show a.r ++ show a.g ++ show a.b

data Direction = Up | Down

data LocalState = LocalState { state :: State
                             , color :: Color
                             , dir   :: Direction }

minColor   = Color { r: 50,  g: 50,  b: 50  }
maxColor   = Color { r: 220, g: 220, b: 220 }
deltaColor = Color { r: 5,   g: 5,   b: 5 }

foreign import fromUiEvent :: forall a e. e -> UIEvent -> Rx.Observable a

setupUI :: forall e. State -> Rx.Observable Action -> String
                  -> Eff ( canvas :: Canvas, dom :: DOM, console :: CONSOLE
                         , random :: RANDOM, ref :: REF | e) (Rx.Observable State)
setupUI state outputActionsStream canvasId = do
    displayBlock canvasId
    Just canvas <- getCanvasElementById canvasId

    offtop <- getElementOffsetTop "canvas"
    offleft <- getElementOffsetLeft "canvas"

    localState <- newRef $ LocalState { state: state, color: minColor, dir: Up }

    let fieldOffsetTop = topOffset + offtop
        fieldOffsetLeft = leftOffset + offleft
        rawClicksStream = fromUiEvent canvas "click"
        pxStream = eventToCoords <$> rawClicksStream
        fieldStream = (coordsInField fieldOffsetLeft fieldOffsetTop) `Rx.filter` pxStream
        cellsClicksStream = (pxToCell fieldOffsetLeft fieldOffsetTop) <$> fieldStream
        inputStateStream = runFn0 newSubject

    cellsClicksStream `Rx.subscribe` postUpstream

    inputStateStream `Rx.subscribe` (\s -> modifyRef localState (\(LocalState ls) -> LocalState (ls {state = s})))
    renderLoop `Rx.subscribe` (renderLocalState canvas localState)

    pure inputStateStream

    where
    renderLocalState :: forall a e. CanvasElement -> Ref LocalState -> a
                                 -> Eff (canvas :: Canvas, console :: CONSOLE, random :: RANDOM, ref :: REF | e) Unit
    renderLocalState canvas ls _ = do
        (LocalState s) <- readRef ls

        let x = stepColor s.color s.dir
        writeRef ls (LocalState (s { color = x.color
                                   , dir = x.dir }))
        renderCanvas canvas s.state s.color

    stepColor :: Color -> Direction -> {color :: Color, dir :: Direction}
    stepColor cur dir =
        case dir of
            Up   -> let newColor = cur + deltaColor
                    in if newColor < maxColor
                           then {color: newColor, dir: Up}
                           else stepColor cur Down
            Down -> let newColor = cur - deltaColor
                    in if newColor > minColor
                           then {color: newColor, dir: Down}
                           else stepColor cur Up

    renderLoop = getIntervalStream 16.0
    postUpstream (Tuple x y) = onNext outputActionsStream $ (TogglePoint y x)

    currentGeneration   = getCurrentGeneration state
    width               = getWidth currentGeneration
    height              = getHeight currentGeneration
    fieldWidth          = width * cellSize
    fieldHeight         = height * cellSize

    eventToCoords :: forall r. {pageX :: Number, pageY :: Number | r} -> Tuple Number Number
    eventToCoords e = Tuple e.pageX e.pageY

    coordsInField :: Int -> Int -> Tuple Number Number -> Boolean
    coordsInField fieldOffsetLeft fieldOffsetTop (Tuple x y) =
            x > (toNumber $ fieldOffsetLeft)
         && x < (toNumber $ fieldOffsetLeft + fieldWidth)
         && y > (toNumber $ fieldOffsetTop)
         && y < (toNumber $ fieldOffsetTop + fieldHeight)

    pxToCell :: Int -> Int -> Tuple Number Number -> Tuple Int Int
    pxToCell fieldOffsetLeft fieldOffsetTop (Tuple x y) =
        let z = Tuple <$> (fromNumber <<< M.floor $ (x - (toNumber fieldOffsetLeft)) / (toNumber cellSize))
                      <*> (fromNumber <<< M.floor $ (y - (toNumber fieldOffsetTop)) / (toNumber cellSize))

        in case z of
            Just t  -> t
            nothing -> Tuple 0 0


renderCanvas :: forall e. CanvasElement -> State -> Color
                       -> Eff (canvas :: Canvas, console :: CONSOLE, random :: RANDOM | e) Unit
renderCanvas canvas state@(State s) color = do
    ctx <- getContext2D canvas

    drawBackground  ctx 0 0 maxX maxY
    drawGrid        ctx width height minX minY maxX maxY
    drawBorders     ctx minX minY maxX maxY
    drawCells       ctx currentGeneration color
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
                         -> Int -> Int -> Int -> Int
                         -> Eff (canvas :: Canvas | e) Unit
drawBackground ctx minX minY maxX maxY = do
    save ctx

    setFillStyle bgColor ctx
    fillPath ctx $ rect ctx { x: (toNumber minX), y: (toNumber minY), w: (toNumber maxX), h: (toNumber maxY) }

    restore ctx
    return unit

drawLabels :: forall e. Context2D -> State ->  Eff (canvas :: Canvas | e) Unit
drawLabels ctx state@(State s) = do
    save ctx
    setFillStyle labelColor ctx
    setFont "16px Source Code Pro" ctx

    fillText ctx (show s.runningState) 5.0 20.0
    fillText ctx ("Time elapsed, s: " ++ show s.secondsElapsed) 5.0 40.0
    fillText ctx ("Gen/sec: " ++ show s.genRatio) 350.0 40.0

    fillText ctx ("Current generation: " ++ (getCurrentGenerationLabel s.current)) 5.0 60.0
    fillText ctx ("Total generations: " ++ show (getTotalGenerations state)) 350.0 60.0

    setFont "12px Source Code Pro" ctx
    fillText ctx ("Space - toggle play/pause, ⟵⟶ - navigate generations, r - random generation.") 5.0 80.0

    restore ctx
    return unit

    where
    getCurrentGenerationLabel x = case x of
        Nothing -> "Latest"
        Just x -> show x

drawBorders :: forall e. Context2D
                      -> Int -> Int -> Int -> Int
                      -> Eff (canvas :: Canvas | e) Unit
drawBorders ctx minX minY maxX maxY = do
    save ctx

    setLineWidth 1.0 ctx
    setStrokeStyle borderColor ctx
    beginPath ctx

    moveTo ctx (toNumber minX) (toNumber minY)
    lineTo ctx (toNumber maxX) (toNumber minY)
    lineTo ctx (toNumber maxX) (toNumber maxY)
    lineTo ctx (toNumber minX) (toNumber maxY)
    lineTo ctx (toNumber minX) (toNumber minY)

    stroke ctx

    restore ctx
    return unit

drawGrid :: forall e. Context2D
                   -> Int -> Int -> Int -> Int ->  Int -> Int
                   -> Eff (canvas :: Canvas | e) Unit
drawGrid ctx x y minX minY maxX maxY = do
    save ctx

    setLineWidth 1.0 ctx
    setStrokeStyle gridColor ctx
    beginPath ctx

    sequence $ map_ (0 .. y) \y' -> do
        moveTo ctx (toNumber minX) (toNumber $ (y' * cellSize) + topOffset)
        lineTo ctx (toNumber maxX) (toNumber $ (y' * cellSize) + topOffset)

    sequence $ map_ (0 .. x) \x' -> do
        moveTo ctx (toNumber $ (x' * cellSize) + leftOffset) (toNumber minY)
        lineTo ctx (toNumber $ (x' * cellSize) + leftOffset) (toNumber maxY)

    stroke ctx

    restore ctx
    return unit

getRandomColor = do
    r <- randomRange 50.0 250.0
    g <- randomRange 50.0 250.0
    b <- randomRange 50.0 250.0
    return $ "#" ++ (hex $ M.floor r) ++ (hex $ M.floor g) ++ (hex $ M.floor b)

drawCells :: forall e. Context2D -> Generation -> Color -> Eff (canvas :: Canvas, random :: RANDOM | e) Unit
drawCells ctx cells cellColor = do
    save ctx
    -- cellColor <- getRandomColor

    for (zip cells (0 .. (length cells))) $ \(Tuple row y) ->
        for (zip row (0 .. (length row))) $ \(Tuple cell x) ->
            case cell of
                Alive -> do
                    drawCell (show cellColor) ctx x y
                Dead -> pure unit

    restore ctx
    return unit

drawCell = drawCircle

drawSquare :: forall e. String -> Context2D -> Int -> Int -> Eff (canvas :: Canvas | e) Unit
drawSquare color ctx x y = do
    save ctx

    setFillStyle color ctx
    fillPath ctx $ rect ctx { x: toNumber ((x * cellSize) + 1 + leftOffset)
                            , y: toNumber ((y * cellSize) + 1 + topOffset)
                            , w: toNumber (cellSize - 1)
                            , h: toNumber (cellSize - 1) }

    restore ctx
    return unit

drawCircle :: forall e. String -> Context2D -> Int -> Int -> Eff (canvas :: Canvas | e) Unit
drawCircle color ctx x y = do
    save ctx

    setFillStyle color ctx

    fillPath ctx $ arc ctx { x     : toNumber $ (x * cellSize) + halfCell + leftOffset
                           , y     : toNumber $ (y * cellSize) + halfCell + topOffset
                           , r     : (toNumber cellSize) / 4.0 + 1.0
                           , start : 0.0
                           , end   : 360.0 }

    restore ctx
    return unit

getWidth :: Generation -> Int
getWidth rows = case rows !! 0 of
    Nothing  -> 0
    Just row -> length row

getHeight :: Generation -> Int
getHeight xs = length xs

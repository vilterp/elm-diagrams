module Diagrams where

{-| Diagrams is a library built on top of Graphics.Collage which allows you to
construct graphics by laying out elements relative to each other. You can also
"tag" elements in the diagram, and, given coordinates (i.e. of the mouse) find
which path in the hierarchy of tagged elements the coordinates are over. Lastly,
given a tag path, you can find the coordinates at which that element was placed.

# Basic Types
@docs Diagram, TagPath, Point

# Rendering and Debugging
@docs render, showBBox, showOrigin, outlineBox

# Properties and Querying
@docs Direction, envelope, width, height, pick, getCoords

# Positioning
@docs beside, above, atop, hcat, vcat, zcat, moveX, moveY, move

# Shortcuts
@docs empty, vspace, hspace, vline, hline

# Geometry Utilities
@docs Transform, applyTrans, invertTrans, magnitude, lerp

-}

import Graphics.Collage as C
import Graphics.Element as E
import Text as T
import List as L
import Maybe as M
import Transform2D
import Color

import Window
import Signal
import Mouse

type alias Point = (Float, Float)

type alias TagPath a = List a

type Diagram a
    -- primitives
    = Circle Float C.FillStyle
    | Rect Float Float C.FillStyle
    | Path (List Point) C.LineStyle
    | Text String T.Style
    | Spacer Float Float
    -- transformation
    | TransformD Transform (Diagram a)
    -- group
    | Group (List (Diagram a))
    -- tag
    | Tag a (Diagram a)

type Transform
    = Translate Float Float
    | Rotate Float
    | Scale Float

-- rendering and debugging

render : Diagram a -> C.Form
render d = case d of
             Tag _ dia -> render dia
             Group dias -> C.group <| L.map render dias
             TransformD (Scale s) dia -> C.scale s <| render dia
             TransformD (Rotate r) dia -> C.rotate r <| render dia
             TransformD (Translate x y) dia -> C.move (x, y) <| render dia
             Spacer _ _ -> C.rect 0 0 |> C.filled Color.black -- TODO: empty graphic?
             Text str ts -> textElem str ts |> C.toForm
             Path path lstyle -> C.traced lstyle path
             Rect w h fstyle -> C.fill fstyle <| C.rect w h
             Circle r fstyle -> C.fill fstyle <| C.circle r

showOrigin : Diagram a -> Diagram a
showOrigin d = let originPoint = Circle 3 (C.Solid Color.red)
               in d `atop` originPoint

showBBox : Diagram a -> Diagram a
showBBox d = let dfl = C.defaultLine
                 style = { dfl | width <- 2
                               , color <- Color.red }
             in outlineBox style d

-- TODO: factor this logic into a bbox function and a outlined rect function
outlineBox : C.LineStyle -> Diagram a -> Diagram a
outlineBox ls dia = let lineWidth = ls.width
                        w = 2*lineWidth + width dia
                        h = 2*lineWidth + height dia
                        horLine = hline w ls
                        vertLine = vline h ls
                        moveLeft = -(envelope dia Left + lineWidth/2)
                        moveRight = envelope dia Right + lineWidth/2
                        moveUp = envelope dia Up + lineWidth/2
                        moveDown = -(envelope dia Down + lineWidth/2)
                        middleH = lerp (moveLeft, moveRight) (0, 1) 0.5
                        middleV = lerp (moveUp, moveDown) (0, 1) 0.5
                        -- TODO: should be possible with simple hcat & vcat
                        vertLineL = move (moveLeft, middleV) vertLine
                        vertLineR = move (moveRight, middleV) vertLine
                        horLineT = move (middleH, moveUp) horLine
                        horLineB = move (middleH, moveDown) horLine
                    in Group [dia, vertLineL, vertLineR, horLineB, horLineT]
                    --in hcat [vertLine, dia, vertLine]

textElem : String -> T.Style -> E.Element
textElem str ts = T.fromString str |> T.style ts |> T.centered

-- properties and querying

type Direction = Up | Down | Left | Right

envelope : Diagram a -> Direction -> Float
envelope dia dir =
    let handleBox w h = case dir of
                          Up -> h/2
                          Down -> h/2
                          Left -> w/2
                          Right -> w/2
    in case dia of
        Tag _ dia' -> envelope dia' dir
        Group dias -> L.maximum <| L.map (\d -> envelope d dir) dias
        TransformD (Scale s) diag -> s * (envelope diag dir)
        -- TODO: TransformD (Rotate r) dia -> (trig!)
        TransformD (Translate tx ty) diag -> let env = envelope diag dir
                                             in case dir of
                                                  Up -> max 0 <| env + ty
                                                  Down -> max 0 <| env - ty
                                                  Right -> max 0 <| env + tx
                                                  Left -> max 0 <| env - tx
        Spacer w h -> handleBox w h
        Text str ts -> let te = textElem str ts
                           width = te |> E.widthOf |> toFloat
                           height = te |> E.heightOf |> toFloat
                       in handleBox width height
        Path path _ -> let xs = L.map fst path
                           ys = L.map snd path
                       in case dir of
                            Left -> L.minimum xs
                            Right -> L.maximum xs
                            Up -> L.maximum ys
                            Down -> L.minimum ys
        Rect w h _ -> handleBox w h
        Circle r _ -> r

width : Diagram a -> Float
width d = (envelope d Left) + (envelope d Right)

height : Diagram a -> Float
height d = (envelope d Up) + (envelope d Down)

-- query

getCoords : Diagram a -> TagPath a -> M.Maybe Point
getCoords dia path = getCoords' dia path (0, 0)

getCoords' : Diagram a -> TagPath a -> Point -> M.Maybe Point
getCoords' diag path start = 
    case path of
      [] -> M.Just start
      (x::xs) -> 
        case diag of
          Tag x dia -> getCoords' dia xs start
          Tag _ _ -> M.Nothing
          Group dias -> firstJust <| L.map (\d -> getCoords' d path start) dias
          TransformD trans dia -> M.map (applyTrans <| invertTrans trans) <| getCoords' dia path start
          _ -> M.Nothing

pick : Diagram a -> Point -> M.Maybe (TagPath a)
pick diag pt =
    let recurse dia pt tagPath = 
          let handleBox (w, h) = let (x, y) = pt
                                     w2 = w/2
                                     h2 = h/2
                                 in if x < w2 && x > -w2 && y < h2 && y > -h2
                                    then M.Just tagPath
                                    else M.Nothing 
          in case dia of
               Circle r _ -> if magnitude pt <= r then M.Just tagPath else M.Nothing
               Rect w h _ -> handleBox (w, h)
               Spacer w h -> handleBox (w, h)
               Path pts _ -> M.Nothing -- TODO implement picking for paths
               Text _ _ -> handleBox (width dia, height dia)
               Group dias -> firstJust <| L.map (\d -> recurse d pt tagPath) dias
               Tag t diagram -> recurse diagram pt (tagPath ++ [t])
               TransformD trans diagram -> recurse diagram (applyTrans (invertTrans trans) pt) tagPath
    in recurse diag pt []

-- positioning

beside : Diagram a -> Diagram a -> Diagram a
beside a b = let xTrans = (envelope a Right) + (envelope b Left)
             in Group [a, TransformD (Translate xTrans 0) b]

above : Diagram a -> Diagram a -> Diagram a
above a b = let yTrans = (envelope a Down) + (envelope b Up)
              in Group [a, TransformD (Translate 0 -yTrans) b]

-- TODO: which is on top of which?
atop : Diagram a -> Diagram a -> Diagram a
atop a b = Group [a, b]

hcat : List (Diagram a) -> Diagram a
hcat = L.foldr beside empty

vcat : List (Diagram a) -> Diagram a
vcat = L.foldl above empty

zcat : List (Diagram a) -> Diagram a
zcat = Group -- lol

moveX : Float -> Diagram a -> Diagram a
moveX x = move (x, 0)

moveY : Float -> Diagram a -> Diagram a
moveY y = move (0, y)

move : (Float, Float) -> Diagram a -> Diagram a
move (x, y) dia = TransformD (Translate x y) dia

-- shortcuts

empty : Diagram a
empty = Spacer 0 0

vspace : Float -> Diagram a
vspace h = Spacer 0 h

hspace : Float -> Diagram a
hspace w = Spacer w 0

vline : Float -> C.LineStyle -> Diagram a
vline h ls = Path [(0, h/2), (0, -h/2)] ls

hline : Float -> C.LineStyle -> Diagram a
hline w ls = Path [(-w/2, 0), (w/2, 0)] ls

-- default styles

defaultFill : C.FillStyle
defaultFill = C.Solid Color.blue

-- TODO: util functions with defaults
-- TODO: default style functions (re-export...)

-- util

applyTrans : Transform -> Point -> Point
applyTrans trans (x, y) = 
  case trans of
    Scale s -> (x*s, y*s)
    Rotate angle -> let c = cos angle
                        s = sin angle
                    in (c*x - s*y, s*x + c*y)
    Translate tx ty -> (x + tx, y + ty)

magnitude : Point -> Float
magnitude (x, y) = sqrt <| (x^2) + (y^2)

invertTrans : Transform -> Transform
invertTrans t = case t of
                  Rotate angle -> Rotate (-angle)
                  Scale factor -> Scale (1/factor)
                  Translate x y -> Translate (-x) (-y)

firstJust : List (M.Maybe a) -> M.Maybe a
firstJust l = case l of
                [] -> M.Nothing
                ((M.Just x)::xs) -> M.Just x
                (_::xs) -> firstJust xs

-- linear interpolation
lerp : (Float, Float) -> (Float, Float) -> Float -> Float
lerp (omin, omax) (imin, imax) input = omin + (omax - omin) * (input - imin) / (imax - imin)

-- TODO: bezier

-- TODO: align top, left, bottom, right
--align : Direction -> [Diagram a] -> Diagram a
--align dir dias = case dir of
--                   Left -> let widths = L.map width dias
--                               maxWidth = L.maximum widths
--                               halfMax = maxWidth/2
--                           in Group movedDias
-- put them beside each other?
-- translation: biggestWidth/2 - selfWidth/2

-- TODO: margin, background (?)

-- TODO triangle

-- TODO: do paths close?

-- outside world utils

toPoint : (Int, Int) -> Point
toPoint (x, y) = (toFloat x, toFloat y)

floatWindowDims = Signal.map toPoint Window.dimensions
floatMousePos = Signal.map toPoint Mouse.position
toCollage (w, h) (x, y) = (x - w/2, h/2 - y)
collageMousePos = Signal.map2 toCollage floatWindowDims floatMousePos

module Diagrams where

import Graphics.Collage as C
import Graphics.Element as E
import Text as T
import List as L
import Maybe as M
import Transform2D
import Color

import Debug

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

textElem : String -> T.Style -> E.Element
textElem str ts = T.fromString str |> T.style ts |> T.centered

-- TODO: implement these in terms of envelope

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

-- shortcuts

vspace : Float -> Diagram a
vspace h = Spacer 0 h

hspace : Float -> Diagram a
hspace w = Spacer w 0

vline : Float -> C.LineStyle -> Diagram a
vline h ls = Path [(0, h/2), (0, -h/2)] ls

hline : Float -> C.LineStyle -> Diagram a
hline w ls = Path [(-w/2, 0), (w/2, 0)] ls

moveX : Float -> Diagram a -> Diagram a
moveX x = move (x, 0)

moveY : Float -> Diagram a -> Diagram a
moveY y = move (0, y)

move : (Float, Float) -> Diagram a -> Diagram a
move (x, y) dia = TransformD (Translate x y) dia

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
                        -- TODO: should be possible with simple hcat & vcat
                        vertLineL = move (moveLeft, h/2) vertLine
                        vertLineR = move (moveRight, h/2) vertLine
                        horLineT = move (w/2, moveUp) horLine
                        horLineB = move (w/2, moveDown) horLine
                    in Group [dia, vertLineL, vertLineR, horLineB, horLineT]
                    --in hcat [vertLine, dia, vertLine]

-- 2nd order

hcat : List (Diagram a) -> Diagram a
hcat = L.foldr1 beside

vcat : List (Diagram a) -> Diagram a
vcat = L.foldl1 above

zcat : List (Diagram a) -> Diagram a
zcat = Group -- lol

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
        case dia of
          Circle r _ -> if magnitude pt <= r then M.Just tagPath else M.Nothing
          Rect w h _ -> let (x, y) = pt
                            w2 = w/2
                            h2 = h/2
                        in if x < w2 && x > -w2 && y < h2 && y > -h2
                           then M.Just tagPath
                           else M.Nothing
          Path pts _ -> M.Nothing -- TODO implement picking for paths
          Text _ _ -> let (x, y) = pt
                          w = width dia
                          h = height dia
                     in if x < (w/2) && y < (h/2) then M.Just tagPath
                                                   else M.Nothing
          Group dias -> firstJust <| L.map (\d -> recurse d pt tagPath) dias
          Tag t diagram -> recurse diagram pt (tagPath ++ [t])
          TransformD trans diagram -> recurse diagram (applyTrans (invertTrans trans) pt) tagPath
    in recurse diag pt []

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

-- debug

showOrigin : Diagram a -> Diagram a
showOrigin d = let originPoint = Circle 3 (C.Solid Color.red)
               in d `atop` originPoint

showBBox : Diagram a -> Diagram a
showBBox d = let dfl = C.defaultLine
                 style = { dfl | width <- 2
                               , color <- Color.red }
             in outlineBox style d

-- TODO: outlined rectangle
-- TODO: bezier
-- TODO: envelope

-- TODO: align top, left, bottom, right

-- TODO triangle

-- TODO: do paths close?

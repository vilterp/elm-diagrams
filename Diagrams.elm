module Diagrams where

import Graphics.Collage as C
import Graphics.Element as E
import Text as T
import List as L
import Maybe as M
import Transform2D
import Color

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

width : Diagram a -> Float
width d = case d of
            Tag _ dia -> width dia
            Group dias -> L.maximum <| L.map width dias
            TransformD (Scale s) dia -> s * (width dia)
            -- TODO: TransformD (Rotate r) dia -> (trig!)
            TransformD (Translate _ _) dia -> width dia
            Spacer w _ -> w
            Text str ts -> toFloat <| E.widthOf <| textElem str ts
            Path path _ -> let xs = L.map fst path
                               maxX = L.maximum xs -- TODO: would be nice to
                               minX = L.minimum xs -- not iterate twice
                           in maxX - minX
            Rect w _ _ -> w
            Circle r _ -> 2 * r

height : Diagram a -> Float
height d = case d of
             Tag _ dia -> height dia
             Group dias -> L.maximum <| L.map height dias
             TransformD (Scale s) dia -> s * (height dia)
             -- TODO: TransformD (Rotate r) dia -> (trig!)
             TransformD (Translate _ _) dia -> height dia
             Spacer _ h -> h
             Text str ts -> toFloat <| E.heightOf <| textElem str ts
             Path path _ -> let ys = L.map snd path
                                maxY = L.maximum <| ys -- TODO: would be nice to
                                minY = L.minimum <| ys -- not iterate twice
                            in maxY - minY
             Rect _ h _ -> h
             Circle r _ -> 2 * r

-- positioning

beside : Diagram a -> Diagram a -> Diagram a
beside a b = let xTrans = (width a)/2 + (width b)/2
             in Group [a, TransformD (Translate xTrans 0) b]

above : Diagram a -> Diagram a -> Diagram a
above a b = let yTrans = (height a)/2 + (height b)/2
              in Group [a, TransformD (Translate 0 yTrans) b]

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

outlineBox : C.LineStyle -> Diagram a -> Diagram a
outlineBox ls dia = let lineWidth = ls.width
                        w = 2*lineWidth + width dia
                        h = 2*lineWidth + height dia
                        horLine = hline w ls
                        vertLine = vline h ls
                    in vcat [
                              --showOrigin horLine
                            hcat [vertLine, dia, vertLine]
                            --, horLine
                            ]

-- 2nd order

hcat : List (Diagram a) -> Diagram a
hcat = L.foldr1 beside

vcat : List (Diagram a) -> Diagram a
vcat = L.foldr1 above

zcat : List (Diagram a) -> Diagram a
zcat = L.foldr1 atop

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
          Circle r _ -> if magnitude pt < r then M.Just tagPath else M.Nothing
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
          TransformD trans diagram -> recurse diagram (applyTrans trans pt) tagPath
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
magnitude (x, y) = sqrt <| (x^2) + (x^2)

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
showOrigin d = let originPoint = Circle 5 (C.Solid Color.red)
               in d `atop` originPoint

showBBox : Diagram a -> Diagram a
showBBox d = let dfl = C.defaultLine
                 style = { dfl | width <- 1
                               , color <- Color.red }
             in outlineBox style d

-- TODO: outlined rectangle
-- TODO: bezier
-- TODO: envelope

-- TODO: align top, left, bottom, right

type Tag = RectA
         | RectB
         | Circ

testDia = let path = Path [(-50,-50), (30, 100)] C.defaultLine
              rectA = Tag RectA <| Rect 50 50 (C.Solid Color.orange)
              rectB = Tag RectB <| Rect 50 50 (C.Solid Color.blue)
              rects = vcat [ rectA , rectB ]
              circ = Tag Circ <| Circle 20 (C.Solid Color.yellow)
          in hcat [ rectA, rectB ]

-- TODO triangle

-- TODO: do paths close?

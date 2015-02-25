module Diagrams where

{-| Diagrams is a library built on top of `Graphics.Collage` which allows you to
construct graphics by laying out elements relative to each other.

A Diagram is represented as a tree of elements, where the leaves are primitive
shapes like rectangles, circles, paths and text and the nodes are transformations
like translation, rotation, and scaling.

There are also `Group` nodes. These have multiple children which are transformed
simultaneously by the transformations above them.

[Sierpinski triangle example](https://gist.github.com/vilterp/9966fd18de8d9b282ade)

Lastly, there are `Tag` nodes which just hold a child diagram and a value of type a;
these exist solely to identify a subdiagram, for the purposes of (a) specifying a tag
path and getting the coordinates it was positioned at (the `getCoords` function) or
(b) given a point, find what subtree it is over (the `pick` function).

Using signals to compose `pick` with mouse clicks, you can create a signal of
clicked-on elements. Folding this with the application state and re-rendering, you
can make an interface which is responsive to the mouse without channels.

The library is based on the excellent [Diagrams][hd] library for Haskell, which
has a nice [visual tutorial][hd-tut]. Things are named slightly differently, and this
version is missing a lot of features and generality.

 [hd]: http://projects.haskell.org/diagrams/
 [hd-tut]: http://projects.haskell.org/diagrams/doc/quickstart.html

# Basic Types
@docs Diagram, TagPath, Point

# Constructors
@docs circle, rect, path, polygon, text, spacer, transform, group, tag, ngon, eqTriangle

# Basic Transforms
@docs move, moveX, moveY, scale, rotate

# Rendering and Debugging
@docs render, showBBox, showOrigin, outlineBox

# Properties and Querying
@docs Direction, envelope, width, height, PickPath, pick, getCoords

# Relative Positioning
@docs beside, above, atop, hcat, vcat, zcat, alignLeft, alignCenter

# Composition Utilities
@docs empty, vspace, hspace, vline, hline, pad, padAll, background

# Styles
@docs FillStroke, justFill, justStroke, fillAndStroke, invisible

# Bezier curves
@docs bezier

# Geometry Utilities
@docs Transform, applyTrans, invertTrans, magnitude, lerp

# Setup Utilities
@docs fullWindowView, fullWindowMain, collageMousePos

-}

import Graphics.Collage as C
import Graphics.Element as E
import Text as T
import List as L
import List ((::))
import Maybe as M
import Transform2D
import Color

import Window
import Signal
import Mouse

import Debug

type alias Point = (Float, Float)

type alias TagPath a = List a

type PathType = ClosedP | OpenP

{-| The recursive tree datatype which represents diagrams. NOTE: because
these may change, use the functions under Constructors to create them,
not the datatype constructors themselves. -}
type Diagram a
    -- primitives
    = Circle Float FillStroke
    | Rect Float Float FillStroke
    | Path (List Point) FillStroke PathType
    | Text String T.Style E.Element
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

type alias FillStroke = { fill : Maybe C.FillStyle
                        , stroke : Maybe C.LineStyle
                        }

-- constructors

-- TODO: shouldn't fill style come first in these? I dunno

{-| Circle with a given radius and fill, centered on the local origin. -}
circle : Float -> FillStroke -> Diagram a
circle = Circle

{-| Rectangle with given width, height, and fill, centered on the local origin. -}
rect : Float -> Float -> FillStroke -> Diagram a
rect = Rect

{-| Unclosed path made of this list of points, laid out relative to the local origin. -}
path : List Point -> C.LineStyle -> Diagram a
path points ls = Path points (justStroke ls) OpenP

polygon : List Point -> FillStroke -> Diagram a
polygon points fs = Path points fs ClosedP

{-| Text with given style, centered vertically and horizontally on the local origin. -}
text : String -> T.Style -> Diagram a
text txt style = let te = textElem txt style
                 in Text txt style te

{-| Spacer with given width and height; renders as transparent. -}
spacer : Float -> Float -> Diagram a
spacer w h = rect w h invisible

{-| Translate, rotate, or scale a given diagram. The transformed diagram has the
same origin. -}
transform : Transform -> Diagram a -> Diagram a
transform = TransformD

{-| Group a list of Diagrams in to one. Elements will be stacked with local origins
on top of one another. This is the same as `zcat`. -}
group : List (Diagram a) -> Diagram a
group = Group

{-| Return a Tag node with the given diagram as its sole child. Adding this to the 
diagram tree is useful for picking and getting coordinates. -}
tag : a -> Diagram a -> Diagram a
tag = Tag

{-| equilateral triangle with given side length & fill/stroke style -}
eqTriangle : Float -> FillStroke -> Diagram a
eqTriangle sideLength fs = ngon 3 sideLength fs

-- adapted from Graphics.Collage
{-| regular polygon with number of sides, side length, & fill/stroke style -}
ngon : Int -> Float -> FillStroke -> Diagram a
ngon n r fs =
  let m = toFloat n
      t = 2 * pi / m
      f i = ( r * cos ((t*i) + pi/2), r * sin ((t*i) + pi/2) )
  in polygon (L.map f [0..m-1]) fs

-- basic transformations

rotate : Float -> Diagram a -> Diagram a
rotate r d = TransformD (Rotate r) d

{-| Translate given diagram by (x, y). Origin of resulting diagram is the same. -}
move : (Float, Float) -> Diagram a -> Diagram a
move (x, y) dia = TransformD (Translate x y) dia

moveX : Float -> Diagram a -> Diagram a
moveX x = move (x, 0)

moveY : Float -> Diagram a -> Diagram a
moveY y = move (0, y)

scale : Float -> Diagram a -> Diagram a
scale s d = TransformD (Scale s) d

-- rendering and debugging

render : Diagram a -> C.Form
render d = let handleFS fs pathType shape =
                 let filled = case fs.fill of
                                Just fillStyle -> [C.fill fillStyle shape]
                                Nothing -> []
                     stroked = case fs.stroke of
                                 Just strokeStyle -> case pathType of
                                                       ClosedP -> [C.outlined strokeStyle shape]
                                                       OpenP -> [C.traced strokeStyle shape]
                                 Nothing -> []
                 in C.group <| stroked ++ filled
           in case d of
                Tag _ dia -> render dia
                Group dias -> C.group <| L.map render <| L.reverse dias -- TODO: this seems semantically right; don't want to
                                                                        -- have to reverse tho
                TransformD (Scale s) dia -> C.scale s <| render dia
                TransformD (Rotate r) dia -> C.rotate r <| render dia
                TransformD (Translate x y) dia -> C.move (x, y) <| render dia
                Text str ts te -> C.toForm te
                Path path fs ty -> handleFS fs ty path
                Rect w h fs -> handleFS fs ClosedP <| C.rect w h
                Circle r fs -> handleFS fs ClosedP <| C.circle r

{-| Draw a red dot at `(0, 0)` in the diagram's local vector space. -}
showOrigin : Diagram a -> Diagram a
showOrigin d = let originPoint = circle 3 <| justFill <| C.Solid Color.red
               in originPoint `atop` d

{-| Draw a red dot box around a diagram. Implemented in terms of `envelope`. -}
showBBox : Diagram a -> Diagram a
showBBox d = let dfl = C.defaultLine
                 style = { dfl | width <- 2
                               , color <- Color.red }
             in outlineBox style d

type alias BBox = { up : Float, down : Float, left : Float, right : Float }
type alias TransWHBox = { translate : (Float, Float), width : Float, height : Float }

bbox2transwh : BBox -> TransWHBox
bbox2transwh bbox = { translate = ((bbox.right - bbox.left)/2, (bbox.up - bbox.down)/2),
                      width = bbox.right + bbox.left,
                      height = bbox.up + bbox.down }

boundingBox : Diagram a -> BBox
boundingBox dia = { up = envelope Up dia
                  , down = envelope Down dia
                  , left = envelope Left dia
                  , right = envelope Right dia
                  }

{-| Draw a box around the given diagram (uses `envelope`) -}
outlineBox : C.LineStyle -> Diagram a -> Diagram a
outlineBox ls dia = background (justStroke ls) <| pad (ls.width/2) dia

textElem : String -> T.Style -> E.Element
textElem str ts = T.fromString str |> T.style ts |> T.centered

-- properties and querying

type Direction = Up | Down | Left | Right

{-| Given a Diagram and a Direction, return the distance in that direction from the origin
to the closest line which doesn't intersect the content of the diagram.

 [hd]: http://projects.haskell.org/diagrams/doc/manual.html#envelopes-and-local-vector-spaces
-}
envelope : Direction -> Diagram a -> Float
envelope dir dia =
    let handleBox w h borderWidth =
          let base = case dir of
                       Up -> h/2
                       Down -> h/2
                       Left -> w/2
                       Right -> w/2
          in base + borderWidth
    in case dia of
        Tag _ dia' -> envelope dir dia'
        Group dias -> L.maximum <| L.map (envelope dir) dias
        TransformD (Scale s) diag -> s * (envelope dir diag)
        TransformD (Rotate r) rotDia ->
            case rotDia of
              Path points fs pt -> let newPoints = L.map (applyTrans <| Rotate r) points
                                   in envelope dir <| Path newPoints fs pt
              Circle _ _ -> envelope dir rotDia
              -- TODO: handleBox for rect, text
        TransformD (Translate tx ty) diag -> let env = envelope dir diag
                                             in case dir of
                                                  Up -> max 0 <| env + ty
                                                  Down -> max 0 <| env - ty
                                                  Right -> max 0 <| env + tx
                                                  Left -> max 0 <| env - tx
        Text str ts te -> handleBox (toFloat <| E.widthOf te) (toFloat <| E.heightOf te) 0
        Path path fs _ -> let xs = L.map fst path
                              ys = L.map snd path
                          in case dir of
                               Left -> -(L.minimum xs)
                               Right -> L.maximum xs
                               Up -> L.maximum ys
                               Down -> -(L.minimum ys)
        Rect w h fs -> handleBox w h (halfStrokeWidth fs)
        Circle r fs -> r + (halfStrokeWidth fs)

width : Diagram a -> Float
width d = (envelope Left d) + (envelope Right d)

height : Diagram a -> Float
height d = (envelope Up d) + (envelope Down d)

-- query

getCoords : Diagram a -> TagPath a -> Maybe Point
getCoords dia path = getCoords' dia path (0, 0)

getCoords' : Diagram a -> TagPath a -> Point -> Maybe Point
getCoords' diag path start = 
    case path of
      [] -> Just start
      (x::xs) -> 
        case diag of
          Tag t dia -> if x == t then getCoords' dia xs start else Nothing
          Group dias -> M.oneOf <| L.map (\d -> getCoords' d path start) dias
          TransformD trans dia -> getCoords' dia path (applyTrans trans start)
          _ -> Nothing

{-| (Tag, Coordinates) pairs from bottom of tree to top; result of
calling `pick` (see below). -}
type alias PickPath a = List (a, Point)

{-| Given a diagram and a point (e.g. of the mouse), return the list of Tag nodes between
the diagram root and the leaf node the point is within (a primitive visual element), along
with the point's offset from the local origin at each level. If the mouse is not over a leaf
node, return `[]`.

Returns a `PickPath`, which is a list of (Tag, Coordinates) pairs ordered
from the leaf node to the root. The second element in each pair is the given
point in the tag's coordinate space -- the point which, if transformed by every
Transform node from that tag node to the root,  would be the point initially given to `pick`.

    pick myDiagram myPoint
    => [(<tag nearest myDiagram leaf>, <myPoint in leaf coordinate space>),
       , ...
       , (<tag nearest myDiagram root>, <myPoint in root coordinate space>)]

-}
pick : Diagram a -> Point -> PickPath a
pick diag pt =
    let recurse dia pt pickPath = 
          let handleBox w h borderWidth =
                let (x, y) = pt
                    w2 = w/2 + borderWidth
                    h2 = h/2 + borderWidth
                in if x < w2 && x > -w2 && y < h2 && y > -h2
                   then pickPath
                   else []
          in case dia of
               Circle r fs -> if magnitude pt <= r + (halfStrokeWidth fs) then pickPath else []
               Rect w h fs -> handleBox w h (halfStrokeWidth fs)
               Path pts _ _ -> [] -- TODO implement picking for paths
               Text _ _ te -> handleBox (toFloat <| E.widthOf te) (toFloat <| E.heightOf te) 0
               Group dias -> firstNonempty <| L.map (\d -> recurse d pt pickPath) dias
               Tag t diagram -> recurse diagram pt ((t, pt) :: pickPath)
               TransformD trans diagram -> recurse diagram (applyTrans (invertTrans trans) pt) pickPath
    in recurse diag pt []

-- positioning

{-| Given two diagrams a and b, place b to the right of a, such that their origins
are on a horizontal line and their envelopes touch. The origin of the new diagram
is the origin of a. -}
beside : Diagram a -> Diagram a -> Diagram a
beside a b = let xTrans = (envelope Right a) + (envelope Left b)
             in Group [a, TransformD (Translate xTrans 0) b]

{-| Given two diagrams a and b, place b to the right of a, such that their origins
are on a horizontal line and their envelopes touch. The origin of the new diagram
is the origin of a. -}
above : Diagram a -> Diagram a -> Diagram a
above a b = let yTrans = (envelope Down a) + (envelope Up b)
              in Group [a, TransformD (Translate 0 -yTrans) b]

{-| Given two diagrams a and b, stack a on top of b in the "out of page" axis,
so a occludes b. -}
atop : Diagram a -> Diagram a -> Diagram a
atop a b = Group [a, b]

{-| Place a list of Diagrams next to each other, such that
their origins are along a horizontal line. The first element in the list will
be on the left; the last on the right. -}
hcat : List (Diagram a) -> Diagram a
hcat = L.foldr beside empty

{-| Place a list of Diagrams next to each other, such that
their origins are along a vertical line. The first element in the list will
be on the top; the last on the bottom. -}
vcat : List (Diagram a) -> Diagram a
vcat = L.foldr above empty

{-| Place a list of diagrams on top of each other, with their origin points
stacked on the "out of page" axis. The first diagram in the list is on top.
This is the same as the `group`. -}
zcat : List (Diagram a) -> Diagram a
zcat = Group -- lol

-- TODO: more aligns

{-| Stack diagrams vertically (as with `vcat`), such that their left edges align.
The origin of the resulting diagram is the origin of the first diagram. -}
alignLeft : List (Diagram a) -> Diagram a
alignLeft dias = let leftEnvelopes = L.map (envelope Left) dias
                     maxLE = L.maximum leftEnvelopes
                     moved = L.map2 (\dia le -> moveX -(maxLE - le) dia) dias leftEnvelopes
                 in vcat moved

{-| translate a diagram such that the envelope in all directions is equal -}
alignCenter : Diagram a -> Diagram a
alignCenter dia = let left = envelope Left dia
                      right = envelope Right dia
                      xTrans = (right - left)/2
                      up = envelope Up dia
                      down = envelope Down dia
                      yTrans = (down-up)/2
                  in move (-xTrans, yTrans) dia

-- shortcuts

empty : Diagram a
empty = spacer 0 0

{-| Vertical spacer of height h -}
vspace : Float -> Diagram a
vspace h = spacer 0 h

{-| Horizontal spacer of width w -}
hspace : Float -> Diagram a
hspace w = spacer w 0

{-| Vertical line -}
vline : Float -> C.LineStyle -> Diagram a
vline h ls = path [(0, h/2), (0, -h/2)] ls

{-| Horizontal line -}
hline : Float -> C.LineStyle -> Diagram a
hline w ls = path [(-w/2, 0), (w/2, 0)] ls

-- Bezier curves

-- adapted from https://gist.github.com/irrwitz/968b9762819974c92c9f
{-| Given four points a, cp1, cp2, b, return path diagram which is a bezier
curve from a to b, using cp1 and cp2 as control points. -}
bezier : Point -> Point -> Point -> Point -> C.LineStyle -> Diagram a
bezier a cp1 cp2 b ls = path (bezierCurve a cp1 cp2 b) ls

bezierCurve : Point -> Point -> Point -> Point -> List Point
bezierCurve a cp1 cp2 b = L.map (\x -> bezierPoint x [a, cp1, cp2, b]) resolution

bezierPoint : Float -> List Point -> Point
bezierPoint t points = if | (L.length points == 1) -> L.head points 
                          | otherwise -> bezierPoint t (L.map2 (interpolatePoint t) points (L.tail points))

interpolatePoint : Float -> Point -> Point -> Point
interpolatePoint t (x0, y0) (x1, y1) = (lerp (x0, x1) (0, 1) t, lerp (y0, y1) (0, 1) t)

{--
  An array [0, 0.01, 0.02, ..., 1] which defines the resolution of the curve
--}
resolution : List Float
resolution = generate 0 1.0 0.01
                     
{--
  generate: Creates a array of floats beginning from the given start value 
  until end value. Values in between are start value plus multiples
  of step value until end is reached or exceeded.
--}
generate : Float -> Float -> Float -> List Float
generate start end step = if | end <= start -> []
                             | start + step >= end -> [start, end]
                             | otherwise -> [start] ++ generate (start + step) end step

{-| Like `padAll`, but with same spacing on all sides. -}
pad : Float -> Diagram a -> Diagram a
pad pd dia = padAll pd pd pd pd dia

{-| Given four numbers up, down, left, and right, put an invisible spacer
behind the given diagram, changing its envelope. -}
padAll : Float -> Float -> Float -> Float -> Diagram a -> Diagram a
padAll u d l r dia =
    let bbox = boundingBox dia
        paddedBbox = { up = bbox.up + u
                     , down = bbox.down + d
                     , left = bbox.left + l
                     , right = bbox.right + r }
        transWH = bbox2transwh paddedBbox
        padder = move (transWH.translate) <| spacer (transWH.width) (transWH.height)
    in zcat [dia, padder]

{-| Put a rectangle behind the given diagram, matching its bounding box. -}
background : FillStroke -> Diagram a -> Diagram a
background fs dia = let transWH = bbox2transwh <| boundingBox dia
                        bg = move transWH.translate <| rect transWH.width transWH.height fs
                    in zcat [dia, bg]

-- Styles

justFill : C.FillStyle -> FillStroke
justFill fs = { fill = Just fs, stroke = Nothing }

justStroke : C.LineStyle -> FillStroke
justStroke ls = { fill = Nothing, stroke = Just ls }

fillAndStroke : C.FillStyle -> C.LineStyle -> FillStroke
fillAndStroke fs ls = { fill = Just fs, stroke = Just ls }

invisible : FillStroke
invisible = { fill = Nothing, stroke = Nothing }

defaultStroke : C.LineStyle
defaultStroke = let defLine = C.defaultLine
                in { defLine | width <- 3
                             , cap <- C.Padded }

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

firstNonempty : List (List a) -> List a
firstNonempty l = case l of
                    [] -> []
                    []::xs -> firstNonempty xs
                    x::xs -> x

halfStrokeWidth : FillStroke -> Float
halfStrokeWidth fs = case fs.stroke of
                       Just ls -> ls.width/2
                       Nothing -> 0
        

-- linear interpolation
{-| linear interpolation. To map x from interval (imin, imax) to (omin, omax), use:

    lerp (omin, omax) (imin, imax) x

-}
lerp : (Float, Float) -> (Float, Float) -> Float -> Float
lerp (omin, omax) (imin, imax) input = omin + (omax - omin) * (input - imin) / (imax - imin)

-- outside world utils

toPoint : (Int, Int) -> Point
toPoint (x, y) = (toFloat x, toFloat y)

floatWindowDims = Signal.map toPoint Window.dimensions
floatMousePos = Signal.map toPoint Mouse.position
toCollageCoords (w, h) (x, y) = (x - w/2, h/2 - y)

{-| Mouse position relative to the collage's origin (in the middle of the screen). -}
collageMousePos : Signal Point
collageMousePos = Signal.map2 toCollageCoords floatWindowDims floatMousePos

fullWindowView : (Int, Int) -> Diagram a -> E.Element
fullWindowView (w, h) d = C.collage w h [render d]

{-| The easiest way to get a diagram on the screen:

    main = fullWindowMain (rect 10 10 (justFill <| C.Solid Color.orange))
-}
fullWindowMain : Diagram a -> Signal E.Element
fullWindowMain dia = Signal.map (\size -> fullWindowView size dia) Window.dimensions

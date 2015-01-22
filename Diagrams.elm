module Diagrams where

{-| Diagrams is a library built on top of Graphics.Collage which allows you to
construct graphics by laying out elements relative to each other.

A Diagram is represented as a tree of elements, where the leaves are primitive
shapes like rectangles, circles, paths and text and the nodes are transformations
like translation, rotation, and scaling.

There are also `Group` nodes have multiple children which are transformed
simultaneously by the transformations above them.

Lastly, there are `Tag` nodes which just hold a child diagram and a value of type a;
these exist solely to identify a subdiagram, for the purposes of (a) specifying a tag
path and getting the coordinates it was positioned at (the `getCoords` function) or
(b) given a point, find what subtree it is over (the `pick` function).

Using signals to compose pick with mouse clicks, you can create a signal of
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
@docs circle, rect, path, text, spacer, transform, group, tag

# Basic Transforms
@docs move, moveX, moveY, scale, rotate

# Rendering and Debugging
@docs render, fullWindowView, fullWindowMain, showBBox, showOrigin, outlineBox

# Properties and Querying
@docs Direction, envelope, width, height, pick, getCoords

# Relative Positioning
@docs beside, above, atop, hcat, vcat, zcat, alignLeft

# Shortcuts
@docs empty, vspace, hspace, vline, hline

# Geometry Utilities
@docs Transform, applyTrans, invertTrans, magnitude, lerp

# Setup Utilities
@docs fullWindowView, fullWindowMain

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

{-| The recursive tree datatype which represents diagrams. NOTE: because
these may change, use the functions under Constructors to create them,
not the datatype constructors themselves. -}
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

-- constructors

{-| Circle with a given radius and fill, centered on the local origin. -}
circle : Float -> C.FillStyle -> Diagram a
circle = Circle

{-| Rectangle with given width, height, and fill, centered on the local origin. -}
rect : Float -> Float -> C.FillStyle -> Diagram a
rect = Rect

{-| Unclosed path made of this list of points, laid out relative to the local origin. -}
path : List Point -> C.LineStyle -> Diagram a
path = Path

{-| Text with given style, centered vertically and horizontally on the local origin. -}
text : String -> T.Style -> Diagram a
text = Text

{-| Spacer with given width and height; renders as transparent. -}
spacer : Float -> Float -> Diagram a
spacer = Spacer

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
render d = case d of
             Tag _ dia -> render dia
             Group dias -> C.group <| L.map render <| L.reverse dias -- TODO: this seems semantically right; don't want to
                                                                     -- have to reverse tho
             TransformD (Scale s) dia -> C.scale s <| render dia
             TransformD (Rotate r) dia -> C.rotate r <| render dia
             TransformD (Translate x y) dia -> C.move (x, y) <| render dia
             Spacer _ _ -> C.rect 0 0 |> C.filled Color.black
             Text str ts -> textElem str ts |> C.toForm
             Path path lstyle -> C.traced lstyle path
             Rect w h fstyle -> C.fill fstyle <| C.rect w h
             Circle r fstyle -> C.fill fstyle <| C.circle r

{-| Draw a red dot at `(0, 0)` in the diagram's local vector space. -}
showOrigin : Diagram a -> Diagram a
showOrigin d = let originPoint = Circle 3 (C.Solid Color.red)
               in originPoint `atop` d

{-| Draw a red dot box around a diagram. Implemented in terms of `envelope`. -}
showBBox : Diagram a -> Diagram a
showBBox d = let dfl = C.defaultLine
                 style = { dfl | width <- 2
                               , color <- Color.red }
             in outlineBox style d

-- TODO: factor this logic into a bbox function and a outlined rect function

{-| Draw a box around the given diagram (uses envelope) -}
outlineBox : C.LineStyle -> Diagram a -> Diagram a
outlineBox ls dia = let lineWidth = ls.width
                        w = 2*lineWidth + width dia
                        h = 2*lineWidth + height dia
                        horLine = hline w ls
                        vertLine = vline h ls
                        moveLeft = -(envelope Left dia + lineWidth/2)
                        moveRight = envelope Right dia + lineWidth/2
                        moveUp = envelope Up dia + lineWidth/2
                        moveDown = -(envelope Down dia + lineWidth/2)
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

{-| Given a Diagram and a Direction, return the distance in that direction from the origin
to the closest line which doesn't intersect the content of the diagram.

 [hd]: http://projects.haskell.org/diagrams/doc/manual.html#envelopes-and-local-vector-spaces
-}
envelope : Direction -> Diagram a -> Float
envelope dir dia =
    let handleBox w h = case dir of
                          Up -> h/2
                          Down -> h/2
                          Left -> w/2
                          Right -> w/2
    in case dia of
        Tag _ dia' -> envelope dir dia'
        Group dias -> L.maximum <| L.map (envelope dir) dias
        TransformD (Scale s) diag -> s * (envelope dir diag)
        -- TODO: TransformD (Rotate r) dia -> (trig!)
        TransformD (Translate tx ty) diag -> let env = envelope dir diag
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
width d = (envelope Left d) + (envelope Right d)

height : Diagram a -> Float
height d = (envelope Up d) + (envelope Down d)

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

{-| Given a diagram and a point (e.g. of the mouse) in that Diagram's coordinate space,
descend the diagram tree to the lowest primitive, returning a list of all tag nodes
the traversal passed through, or M.Nothing if the point was not over any primitives. -}
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
so a occlodes b. -}
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

-- shortcuts

empty : Diagram a
empty = Spacer 0 0

{-| Vertical spacer of height h -}
vspace : Float -> Diagram a
vspace h = Spacer 0 h

{-| Horizontal spacer of width w -}
hspace : Float -> Diagram a
hspace w = Spacer w 0

{-| Vertical line -}
vline : Float -> C.LineStyle -> Diagram a
vline h ls = Path [(0, h/2), (0, -h/2)] ls

{-| Horizontal line -}
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
{-| linear interpolation. To map x from interval (imin, imax) to (omin, omax), use:

    lerp (omin, omax) (imin, imax) x

-}
lerp : (Float, Float) -> (Float, Float) -> Float -> Float
lerp (omin, omax) (imin, imax) input = omin + (omax - omin) * (input - imin) / (imax - imin)

-- TODO triangle

-- outside world utils

toPoint : (Int, Int) -> Point
toPoint (x, y) = (toFloat x, toFloat y)

floatWindowDims = Signal.map toPoint Window.dimensions
floatMousePos = Signal.map toPoint Mouse.position
toCollageCoords (w, h) (x, y) = (x - w/2, h/2 - y)
collageMousePos = Signal.map2 toCollageCoords floatWindowDims floatMousePos

fullWindowView : (Int, Int) -> Diagram a -> E.Element
fullWindowView (w, h) d = C.collage w h [render d]

{-| The easiest way to get a diagram on the screen:

    main = fullWindowMain (rect 10 10 (C.Solid Color.orange))
-}
fullWindowMain : Diagram a -> Signal E.Element
fullWindowMain dia = Signal.map (\size -> fullWindowView size dia) Window.dimensions

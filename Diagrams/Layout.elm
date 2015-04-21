module Diagrams.Layout where

{-| A utility for laying out variable-width diagrams.

TODO: generalize to vertical

# Types
@docs FlexDiagram, Width, LayoutRow

# Layout
@docs layout

# Primitives
@docs spring, block, expando, strut

# Helpers
@docs centered, flexLeft, flexRight, flexCenter, flexAll, hrule

-}

import Graphics.Collage as C
import List as L
import Maybe as M

import Diagrams.Core exposing (..)
import Diagrams.Envelope exposing (..)
import Diagrams.Align exposing (..)
import Diagrams.Pad exposing (..)

type alias Width = Float

{-| A diagram which can vary its width -- e.g. a spacer or a video
player timeline. -}
type FlexDiagram t a
    = Block (Diagram t a) Width
    | Expando { minWidth : Width
              , render : Width -> Diagram t a
              }

type alias LayoutRow t a = List (FlexDiagram t a)

-- layout

{-| Given a list of rows of where some of the diagrams in each row
can have variable width, stack the rows vertically and decide the width of each
diagram such that the width of the whole thing is minimized. -}
layout : List (LayoutRow t a) -> Diagram t a
layout rows = let decidedWidth = L.map rowMinWidth rows |> L.maximum |> M.withDefault 0
              in vcatA LeftA <| L.map (renderRow decidedWidth) rows

fdMinWidth : FlexDiagram t a -> Width
fdMinWidth fd = case fd of
                Block _ width -> width
                Expando attrs -> attrs.minWidth

rowMinWidth : LayoutRow t a -> Width
rowMinWidth fds = L.sum <| L.map fdMinWidth fds

renderRow : Width -> LayoutRow t a -> Diagram t a
renderRow decidedWidth row =
    let hardMins = L.map fdMinWidth row
        count fd = case fd of -- TODO this is dumb
                     Block _ _ -> 0
                     _ -> 1
        numExpandos = L.sum <| L.map count row
        leftOver = decidedWidth - (L.sum hardMins)
        eachExpando = if numExpandos > 0 then leftOver / (toFloat numExpandos) else 0
        renderFD fd = case fd of
                        Block dia _ -> dia
                        Expando {minWidth, render} -> render (minWidth + eachExpando)
    in hcat <| L.map renderFD row

-- primitives

{-| An invisible spacer which will expand horizontally as far as possible. -}
spring : FlexDiagram t a
spring = Expando { minWidth = 0, render = hspace }

{-| A fixed-size diagram. -}
block : Diagram t a -> FlexDiagram t a
block dia = Block dia (width dia)

{-| A diagram which has a minimum width, but can be drawn at any width >= that. -}
expando : Width -> (Width -> Diagram t a) -> FlexDiagram t a
expando w f = Expando { minWidth = w, render = f }

{-| A fixed-width spacer. -}
strut : Width -> FlexDiagram t a
strut w = Block (hspace w) w

-- helpers

{-| A row in which the given diagram will stay centered. -}
centered : Diagram t a -> LayoutRow t a
centered dia = [spring, block dia, spring]

{-| A row in which the given diagram will stay glued to the right:
the left side is flexible. -}
flexLeft : Diagram t a -> LayoutRow t a
flexLeft dia = [spring, block dia]

{-| A row in which the given diagram will stay glued to the left:
the right side is flexible. -}
flexRight : Diagram t a -> LayoutRow t a
flexRight dia = [block dia, spring]

{-| A row in which the given diagrams will stay glued to the left and right:
the center is flexible. -}
flexCenter : Diagram t a -> Diagram t a -> LayoutRow t a
flexCenter ldia rdia = [block ldia, spring, block rdia]

{-| A row containing a diagram which fills the full width of the row. -}
flexAll : (Width -> Diagram t a) -> LayoutRow t a
flexAll f = [expando 0 f]

-- TODO: this should be camel case -- have to change hspace, etc too
{-| Horizontal rule, with given vertical padding on top and bottom.
(Kind of like the `<hr/>` element in HTML) -}
hrule : C.LineStyle -> Float -> LayoutRow t a
hrule ls vPadding = flexAll (\w -> padSpecific vPadding vPadding 0 0 <| hline w ls)

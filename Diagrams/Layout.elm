module Diagrams.Layout where

{-|
TODO: generalize to vertical
# Types
@docs FlexDiagram, Width, LayoutRow

# Layout
@docs layout

# Primitives
@docs spring, block, expando, strut

# Helpers
@docs centered, flexLeft, flexRight, flexCenter, flexAll, hRule

-}

import Graphics.Collage as C
import List as L

import Debug

import Diagrams.Core (..)
import Diagrams.Envelope (..)
import Diagrams.Align (..)
import Diagrams.Debug (..)

type alias Width = Float

type FlexDiagram t a
    = Block (Diagram t a) Width
    | Expando { minWidth : Width
              , render : Width -> Diagram t a
              }

type alias LayoutRow t a = List (FlexDiagram t a)

-- layout

layout : List (LayoutRow t a) -> Diagram t a
layout rows = let decidedWidth = L.maximum <| L.map rowMinWidth rows
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
        tup = Debug.log "tup" (numExpandos, row)
        leftOver = decidedWidth - (L.sum hardMins)
        eachExpando = if numExpandos > 0 then leftOver / (toFloat numExpandos) else 0
        renderFD fd = case fd of
                        Block dia _ -> dia
                        Expando {minWidth, render} -> render (minWidth + eachExpando)
    in hcat <| L.map renderFD row

-- primitives
spring : FlexDiagram t a
spring = Expando { minWidth = 0, render = hspace }

block : Diagram t a -> FlexDiagram t a
block dia = Block dia (width dia)

expando : Width -> (Width -> Diagram t a) -> FlexDiagram t a
expando w f = Expando { minWidth = w, render = f }

strut : Width -> FlexDiagram t a
strut w = Block (hspace w) w

-- helpers

centered : Diagram t a -> LayoutRow t a
centered dia = [spring, block dia, spring]

flexLeft : Diagram t a -> LayoutRow t a
flexLeft dia = [spring, block dia]

flexRight : Diagram t a -> LayoutRow t a
flexRight dia = [block dia, spring]

flexCenter : Diagram t a -> Diagram t a -> LayoutRow t a
flexCenter ldia rdia = [block ldia, spring, block rdia]

flexAll : (Width -> Diagram t a) -> LayoutRow t a
flexAll f = [expando 0 f]

hRule : C.LineStyle -> LayoutRow t a
hRule ls = flexAll (\w -> hline w ls)

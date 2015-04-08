module Tooltip where

import Diagrams.Core (..)
import Diagrams.Align (..)
import Diagrams.Pad (..)
import Diagrams.FillStroke (..)
import Diagrams.FullWindow (..)
import Diagrams.Envelope (..)
import Diagrams.Debug (..)

import Color
import Text as T

import Debug

defText = T.defaultStyle

tooltip : Diagram t a -> FillStroke -> Diagram t a
tooltip dia fs = (background fs (dia |> pad 3))
                  `above` (eqTriangle 7 fs |> rotate (pi/3))
                  |> alignBottom |> moveY 10

main = tooltip (text "Hello World" { defText | color <- Color.white, height <- Just 11 }) (justSolidFill Color.black)
        |> showOrigin
        |> showBBox
        |> fullWindowMain

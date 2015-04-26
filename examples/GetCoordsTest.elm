module GetCoordsTest where

import Color
import Debug
import Diagrams.Core exposing (..)
import Diagrams.Align exposing (..)
import Diagrams.FillStroke exposing (..)
import Diagrams.Query exposing (..)
import Diagrams.FullWindow exposing (..)
import Signal
import Window

type BoxTag = OuterBox | InnerBox

innerBox = (tag InnerBox <| rect 60 30 (justFill <| Solid Color.blue)) |> rotate 30 |> move (-50,60)
outerBox = tag OuterBox <| rect 100 120 (justFill <| Solid Color.yellow)

test : Diagram BoxTag a
test =
    let dia = innerBox `atop` outerBox
        dummy = Debug.log "coords" <| getCoords dia [InnerBox]
    in dia

main = fullWindowMain test


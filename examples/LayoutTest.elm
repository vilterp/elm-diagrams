module LayoutTest where

import Color
import List as L

import Diagrams.Core (..)
import Diagrams.Layout (..)
import Diagrams.FillStroke (..)
import Diagrams.FullWindow (..)

orangeFill = justSolidFill Color.orange
blueFill = justSolidFill Color.blue
greenFill = justSolidFill Color.green

title = rect 100 10 blueFill
xGlyph = rect 10 10 greenFill

topRow = flexCenter title xGlyph

makeText w = rect w 10 orangeFill
inPorts = L.map (flexRight << makeText) [50, 60, 10, 100]
outPorts = L.map (flexLeft << makeText) [120, 5, 80, 75]

allRows = [topRow] ++ inPorts ++ outPorts

dia = layout allRows

main = fullWindowMain dia

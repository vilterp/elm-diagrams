module Basic where

import Diagrams (..)
import Window
import Mouse
import Graphics.Element as E
import Graphics.Collage as C
import Signal
--import Mouse
import Color
import Debug
import Text as T
import List as L

type Tag = RectOrange
         | RectBlue
         | Circ
         | Textt

defLine = C.defaultLine

testDia = let aPath = path [(-50,-50), (30, 100)] C.defaultLine
              rectOrange = tag RectOrange <| rect 50 70 (fillAndStroke (C.Solid Color.orange) { defLine | width <- 20, cap <- C.Padded })
              rectBlue = tag RectBlue <| rect 70 50 (justFill <| C.Solid Color.blue)
              rects = vcat [ rectOrange , rectBlue ]
              circ = tag Circ <| circle 20 (justFill <| C.Solid Color.yellow)
              someText = tag Textt <| text "Foo" (let ds = T.defaultStyle in {ds | bold <- True})
              stuff = circ `atop` (rectOrange `above` (rectBlue `beside` (circ `above` someText)))
              moreStuff = hcat <| L.intersperse circ (L.repeat 5 rectOrange)
          in showOrigin <| showBBox <| alignCenter <|  (stuff `above` stuff `above` moreStuff)

view (w, h) (x, y) = C.collage w h [render testDia]

over mousePt = Debug.watch "over" <| pick testDia (Debug.watch "mouse" mousePt)

overSignal = Signal.map over collageMousePos

main = fullWindowMain testDia

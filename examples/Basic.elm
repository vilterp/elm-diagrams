module Test where

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

testDia = let path = Path [(-50,-50), (30, 100)] C.defaultLine
              rectOrange = Tag RectOrange <| Rect 50 70 (C.Solid Color.orange)
              rectBlue = Tag RectBlue <| Rect 70 50 (C.Solid Color.blue)
              rects = vcat [ rectOrange , rectBlue ]
              circ = Tag Circ <| Circle 20 (C.Solid Color.yellow)
              text = Tag Textt <| Text "Foo" (let ds = T.defaultStyle in {ds | bold <- True})
          --in showBBox <| vcat [ showBBox <| showOrigin <| circ
          --        , showOrigin <| hcat [rectOrange, rectBlue]
          --        , text ]
              stuff = circ `atop` (rectOrange `above` (rectBlue `beside` (circ `above` text)))
              moreStuff = showBBox <| showOrigin <| hcat <| L.intersperse circ (L.repeat 5 rectOrange)
          in showBBox <| showOrigin <| (stuff `above` stuff `above` moreStuff)

view (w, h) (x, y) = C.collage w h [render testDia]

over mousePt = Debug.watch "over" <| pick testDia (Debug.watch "mouse" mousePt)

overSignal = Signal.map over collageMousePos

main = fullWindowMain testDia

module Basic where

import Diagrams.Core (..)
import Diagrams.Query (..)
import Diagrams.Interact (..)
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

type Action = ClickCirc Point
            | EnterOrange Point
            | LeaveOrange Point
            | MoveBlue Point

defLine = C.defaultLine

testDia = let aPath = path [(-50,-50), (30, 100)] C.defaultLine
              rectOrange = tagWithActions RectOrange
                              { emptyActionSet | mouseEnter <- Just EnterOrange
                                               , mouseLeave <- Just LeaveOrange }
                              <| rect 50 70 (fillAndStroke (C.Solid Color.orange) { defLine | width <- 20, cap <- C.Padded })
              rectBlue = tagWithActions RectBlue
                              { emptyActionSet | mouseMove <- Just MoveBlue }
                              <| rect 70 50 (justFill <| C.Solid Color.blue)
              rects = vcat [ rectOrange , rectBlue ]
              circ = tagWithActions Circ
                            { emptyActionSet | click <- Just ClickCirc }
                            <| circle 20 (fillAndStroke (C.Solid Color.yellow) { defLine | width <- 2, cap <- C.Padded })
              someText = tag Textt <| text "Foo" (let ds = T.defaultStyle in {ds | bold <- True})
              stuff = circ `atop` (rectOrange `above` (rectBlue `beside` (circ `above` someText)))
              moreStuff = hcat <| L.intersperse circ (L.repeat 5 rectOrange)
          in showOrigin <| showBBox <| alignCenter <|  (stuff `above` stuff `above` moreStuff)

view (w, h) (x, y) = C.collage w h [render testDia]

locDia = Signal.map (\loc -> { loc = loc, diagram = testDia }) fullWindowCollageLoc

updates : Signal (LocatedDiagram Tag Action, MouseEvent)
updates = Signal.map2 (\loc mouseEvt -> ({loc = loc, diagram = testDia}, mouseEvt)) fullWindowCollageLoc (mouseEvents fullWindowCollageLoc)

upstate : (LocatedDiagram Tag Action, MouseEvent) -> MouseState Tag Action -> MouseState Tag Action
upstate (ld, evt) mst = let (newMst, actions) = process ld mst evt
                            watched = Debug.watch "actions" actions
                            watched2 = Debug.watch "mst" newMst
                        in newMst

state : Signal (MouseState Tag Action)
state = Signal.foldp upstate initMouseState updates

main = fullWindowMain testDia

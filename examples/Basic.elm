module Basic where

import Graphics.Element as E
import Graphics.Collage as C
import Signal
import Window

import Color
import Debug
import Text as T
import List as L

-- whoooo all the moduless
import Diagrams.Core (..)
import Diagrams.Query (..)
import Diagrams.Interact (..)
import Diagrams.Wiring (..)
import Diagrams.Geom (..)
import Diagrams.Debug (..)
import Diagrams.Align (..)
import Diagrams.Pad (..)
import Diagrams.Actions (..)
import Diagrams.FillStroke (..)
import Diagrams.FullWindow (..)

type Tag = RectOrange
         | RectBlue
         | Circ
         | Textt

type Action = ClickCirc Point
            | EnterOrange Point
            | LeaveOrange Point
            | MoveBlue Point

defLine = C.defaultLine

testDia : Diagram Tag Action
testDia = let aPath = path [(-50,-50), (30, 100)] C.defaultLine
              rectOrange = tagWithActions RectOrange
                              { emptyActionSet | mouseEnter <- Just <| keepBubbling <| (\(MouseEvent evt) -> EnterOrange evt.offset)
                                               , mouseLeave <- Just <| keepBubbling <| (\(MouseEvent evt) -> LeaveOrange evt.offset) }
                              <| rect 50 70 (fillAndStroke (C.Solid Color.orange) { defLine | width <- 20, cap <- C.Padded })
              rectBlue = tagWithActions RectBlue
                              { emptyActionSet | mouseMove <- Just <| keepBubbling <| (\(MouseEvent evt) -> MoveBlue evt.offset) }
                              <| rect 70 50 (justSolidFill Color.blue)
              rects = vcat [ rectOrange , rectBlue ]
              circ = tagWithActions Circ
                            { emptyActionSet | click <- Just <| keepBubbling <| (\(MouseEvent evt) -> ClickCirc evt.offset) }
                            <| circle 20 (fillAndStroke (C.Solid Color.yellow) { defLine | width <- 2, cap <- C.Padded })
              justText = text "Foo" (let ds = T.defaultStyle in {ds | bold <- True})
              someText = tag Textt <| background (justSolidFill Color.lightBlue) <| pad 5 <| justText
              stuff = circ `atop` (rectOrange `above` (rectBlue `beside` (circ `above` someText)))
              moreStuff = hcat <| L.intersperse circ (L.repeat 5 rectOrange)
          in showOrigin <| showBBox <| alignCenter <| (stuff `above` stuff `above` moreStuff)

type alias Model = ()

renderF : RenderFunc Model Tag Action
renderF _ = testDia

updateF : UpdateFunc Model Action
updateF _ m = m

initModel : Model
initModel = ()

diagrams : Signal (Diagram Tag Action)
diagrams = interactFold updateF renderF fullWindowCollageLocFunc initModel

main = Signal.map2 fullWindowView Window.dimensions diagrams

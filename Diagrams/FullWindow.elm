module Diagrams.FullWindow where

import Window
import Signal as S
import Graphics.Element as E
import Graphics.Collage as C

import Diagrams.Wiring (..)
import Diagrams.Core (..)

-- useful defaults
-- for when you just want to get a diagram on the whole screen

fullWindowCollageLoc : CollageLocFunc
fullWindowCollageLoc dims = { offset = (0.0,0.0), dims = dims }

fullWindowUpdates : Signal (CollageLocation, MouseEvent)
fullWindowUpdates = makeUpdateStream fullWindowCollageLoc

{-| The easiest way to get a diagram on the screen:

    main = fullWindowMain (rect 10 10 (justFill <| C.Solid Color.orange))
-}
fullWindowMain : Diagram t a -> Signal E.Element
fullWindowMain dia = S.map (\dims -> fullWindowView dims dia) Window.dimensions

fullWindowView : (Int, Int) -> Diagram t a -> E.Element
fullWindowView (w, h) d = C.collage w h [render d]

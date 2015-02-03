module Test where

import Diagrams (..)
import Color
import Graphics.Collage as C
import Signal
import Time
import Window

sierpinski n sl = case n of
                    0 -> eqTriangle sl C.defaultLine
                    _ -> let smaller = sierpinski (n-1) sl
                         in smaller `above`
                            (alignCenter <| smaller `beside` smaller)

-- not spinning
--main = fullWindowMain <| alignCenter <| sierpinski 3 20

framesPast = Signal.foldp (\_ st -> st + 1) 0 (Time.fps 30)
framesPerRotation = 100
angle = Signal.map (\past -> ((toFloat <| past % framesPerRotation) / framesPerRotation) * (2*pi)) framesPast

-- spinning
main = let sier = showOrigin <| alignCenter <| sierpinski 5 20
       in Signal.map2 (\wh ang -> fullWindowView wh <| rotate ang sier) Window.dimensions angle

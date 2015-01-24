module Test where

import Diagrams (..)
import Color
import Graphics.Collage as C

sierpinski n sl = case n of
                    0 -> eqTriangle sl C.defaultLine
                    _ -> let smaller = sierpinski (n-1) sl
                         in smaller `above`
                            (alignCenter <| smaller `beside` smaller)

main = fullWindowMain <| showOrigin <| alignCenter <| sierpinski 5 20

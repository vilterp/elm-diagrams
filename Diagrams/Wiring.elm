module Diagrams.Wiring where

import Signal as S
import Mouse
import Window
import Graphics.Collage as C

import Diagrams.Geom (..)

{-| Where offset is difference between top left of screen and top left of collage,
increasing right and down. -}
type alias CollageLocation = OffsetDimsBox

type alias MouseEvent = (MouseEvtType, Point)

type MouseEvtType = MouseUp
                  | MouseDown
                  | MouseMove

type alias CollageLocFunc = Dims -> CollageLocation

-- TODO: clip events that aren't within the collage loc
makeUpdateStream : CollageLocFunc -> Signal (CollageLocation, MouseEvent)
makeUpdateStream clf =
    let collageLocs = S.map clf floatWindowDims
        mouseEvts = mouseEvents collageLocs
    in S.map2 (,) collageLocs mouseEvts

mouseEvents : Signal CollageLocation -> Signal MouseEvent
mouseEvents loc =
    let upDown = S.map (\down -> if down then MouseDown else MouseUp) Mouse.isDown
        moves = S.map (always MouseMove) Mouse.position
        events = S.merge upDown moves
        adjustedMousePos = S.map2 offsetMousePos loc floatMousePos
    in S.map2 (,) events adjustedMousePos

{-| Given the position of the top-left of a collage (from the top-left of the screen; coords increasing right and down)
and the dimensions of the collage, return a signal of the mouse position relative to the center of that collage.
(Doesn't actually have to be a collage) -}
offsetMousePos : CollageLocation -> Point -> Point
offsetMousePos loc (x, y) = let (offsetX, offsetY) = loc.offset
                                {width, height} = loc.dims
                            in (x - width/2 - offsetX, (height/2 + offsetY) - y)

-- input signals

toPoint : (Int, Int) -> Point
toPoint (x, y) = (toFloat x, toFloat y)

floatMousePos : Signal Point
floatMousePos = S.map toPoint Mouse.position

floatWindowDims : Signal Dims
floatWindowDims = S.map (\(w, h) -> { width = toFloat w, height = toFloat h })
                        Window.dimensions

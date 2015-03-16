module Diagrams.Interact where

{-| Abstractions for making diagrams which change as a function of the mouse.

-}

import Signal as S
import Window
import Mouse

import List as L
import Graphics.Element as E
import Graphics.Collage as C

import Diagrams.Core (..)
import Diagrams.Query (..)

import Debug

type alias MouseEvent = (MouseEvtType, Point)

type MouseEvtType = MouseUp
                  | MouseDown
                  | MouseMove

-- TODO: tagging function easier than `tagWithAction tag { emptyActionSet | click <- Just ... } dia`?
-- like `clickable tag func dia` or something
-- or list of attributes like html

-- BUG: if A is on top of and within B, entering A should not count as leaving B.
-- shit, I guess the pick path should really be a pick tree. #@$@

-- TODO: keep both unzipped for perforance?
type alias MouseState t a = { isDown : Bool, overPath : PickPath t a, overTags : List t }

type alias LocatedDiagram t a = { diagram : Diagram t a, loc : CollageLocation }

type alias CollageLocation = { offset : Point, dimensions : Point }

initMouseState = { isDown = False, overPath = [], overTags = [] }

-- TODO: factor out all the `(\(_, as, pt) -> (pt, as.mouseEnter))`'s.
{-| Given diagram with mouse state (`MouseDiagram`), mouse event, and dimensions of collage, return
new `MouseDiagram` with list of actions triggered by this mouse event. -}
process : LocatedDiagram t a -> MouseState t a -> MouseEvent -> (MouseState t a, List a)
process ldia mouseState (evt, mousePos) =
    let applyActions = L.map (\(pt, e2a) -> e2a pt)
    in case evt of
         MouseDown -> let actions = L.filterMap (getOffsetAndMember .mouseDown) <| mouseState.overPath
                      in ( { mouseState | isDown <- True }
                         , applyActions actions
                         )
         MouseUp -> let mouseUps = L.filterMap (getOffsetAndMember .mouseUp) mouseState.overPath
                        clicks = Debug.watch "clicks" <| L.filterMap (getOffsetAndMember .click) mouseState.overPath
                    in ( { mouseState | isDown <- False }
                       , applyActions <| mouseUps ++ clicks
                       )
         MouseMove -> let -- hate to build set like this on every move
                          overPath = pick ldia.diagram mousePos
                          oldOverPath = mouseState.overPath
                          -- sets of tags of elements mouse has left or entered
                          overTags = L.map .tag overPath
                          oldOverTags = mouseState.overTags
                          -- action sets corresponding to these tags
                          enters = L.filterMap (getOffsetAndMember .mouseEnter) <|
                                      L.filter (\ppe -> not <| L.member ppe.tag oldOverTags) overPath
                          leaves = L.filterMap (getOffsetAndMember .mouseLeave) <|
                                      L.filter (\ppe -> not <| L.member ppe.tag overTags) oldOverPath
                          moves = L.filterMap (getOffsetAndMember .mouseMove) <|
                                      L.filter (\ppe -> L.member ppe.tag oldOverTags) overPath
                      in ( { mouseState | overPath <- overPath
                                        , overTags <- overTags }
                         , applyActions <| enters ++ leaves ++ moves
                         )

-- helper for process
getOffsetAndMember : (ActionSet a -> Maybe (EventToAction a)) -> PickPathElem t a -> Maybe (Point, EventToAction a)
getOffsetAndMember getMember ppe = case getMember ppe.actionSet of
                                     Just e2a -> Just (ppe.offset, e2a)
                                     Nothing -> Nothing

-- TODO: make easy to drop into foldp

-- outside world utils

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
                                (width, height) = loc.dimensions
                            in (x - width/2 - offsetX, (height/2 + offsetY) - y)

fullWindowCollageLoc : Signal CollageLocation
fullWindowCollageLoc = S.map (\dims -> { offset = (0.0,0.0), dimensions = dims }) floatWindowDims

fullWindowMousePos = S.map2 offsetMousePos fullWindowCollageLoc floatMousePos

{-| The easiest way to get a diagram on the screen:

    main = fullWindowMain (rect 10 10 (justFill <| C.Solid Color.orange))
-}
fullWindowMain : Diagram t a -> Signal E.Element
fullWindowMain dia = S.map (\dims -> fullWindowView dims dia) Window.dimensions

fullWindowView : (Int, Int) -> Diagram t a -> E.Element
fullWindowView (w, h) d = C.collage w h [render d]

-- most basic stuff

toPoint : (Int, Int) -> Point
toPoint (x, y) = (toFloat x, toFloat y)

floatMousePos = S.map toPoint Mouse.position
floatWindowDims = S.map toPoint Window.dimensions

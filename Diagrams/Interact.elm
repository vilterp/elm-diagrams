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
import Diagrams.Geom (..)
import Diagrams.Actions (..)
import Diagrams.Wiring (..)

import Debug

-- BUG: if A is on top of and within B, entering A should not count as leaving B.
-- shit, I guess the pick path should really be a pick tree. #@$@

-- TODO: keep both unzipped for perforance?
type alias MouseState t a = { isDown : Bool, overPath : PickPath t a, overTags : List t }

initMouseState = { isDown = False, overPath = [], overTags = [] }

type alias InteractionState m t a =
    { mouseState : MouseState t a
    , loc : CollageLocation
    , diagram : Diagram t a
    , modelState : m
    }

type alias RenderFunc m t a = m -> Dims -> Diagram t a
type alias UpdateFunc m a = a -> m -> m
type alias InteractUpdateFunc m t a = (CollageLocation, MouseEvent) -> InteractionState m t a -> InteractionState m t a

{-| Top-level interface to this module. Given
- how to update the state (type `m`) given an action (type `a`),
- how to render a diagram given the state and the collage dimensions,
- and how to compute the location of the collage on screen from the window dimensions,
Return a signal of diagrams.
-}
interactFold : UpdateFunc m a -> RenderFunc m t a -> CollageLocFunc -> m -> CollageLocation -> Signal (InteractionState m t a)
interactFold updateF renderF collageLocF initModel initLoc =
    S.foldp (makeFoldUpdate updateF renderF)
            (initInteractState renderF initModel initLoc)
            (makeUpdateStream collageLocF)

makeFoldUpdate : UpdateFunc m a -> RenderFunc m t a -> InteractUpdateFunc m t a
makeFoldUpdate updateF renderF =
    \(loc, evt) intState ->
        let (newMS, actions) = processMouseEvent intState.diagram intState.mouseState evt
            watched = Debug.watch "actions" actions
            -- new model
            oldModel = intState.modelState
            newModel = L.foldr updateF oldModel actions
            -- re-render
            oldDiagram = intState.diagram
            newDiagram = if oldModel == newModel
                         then oldDiagram
                         else renderF newModel loc.dims
        in { mouseState = newMS
           , diagram = newDiagram
           , loc = loc
           , modelState = newModel
           }

initInteractState : RenderFunc m t a -> m -> CollageLocation -> InteractionState m t a
initInteractState render model loc =
    { mouseState = initMouseState
    , modelState = model
    , loc = loc
    , diagram = render model loc.dims
    }

{-| Given diagram with mouse state (`MouseDiagram`), mouse event, and dimensions of collage, return
new `MouseDiagram` with list of actions triggered by this mouse event. -}
processMouseEvent : Diagram t a -> MouseState t a -> MouseEvent -> (MouseState t a, List a)
processMouseEvent diagram mouseState (evt, mousePos) =
    let applyActions = L.map (\(pt, e2a) -> e2a pt)
    in case evt of
         MouseDown -> let actions = L.filterMap (getOffsetAndMember .mouseDown) <| mouseState.overPath
                      in ( { mouseState | isDown <- True }
                         , applyActions actions
                         )
         MouseUp -> let mouseUps = L.filterMap (getOffsetAndMember .mouseUp) mouseState.overPath
                        clicks = L.filterMap (getOffsetAndMember .click) mouseState.overPath
                    in ( { mouseState | isDown <- False }
                       , applyActions <| mouseUps ++ clicks
                       )
         MouseMove -> let -- hate to build set like this on every move
                          overPath = pick diagram mousePos
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

-- helper for processMouseEvent
getOffsetAndMember : (ActionSet a -> Maybe (EventToAction a)) -> PickPathElem t a -> Maybe (Point, EventToAction a)
getOffsetAndMember getMember ppe = case getMember ppe.actionSet of
                                     Just e2a -> Just (ppe.offset, e2a)
                                     Nothing -> Nothing

toCollage : InteractionState m t a -> E.Element
toCollage intState = let w = round intState.loc.dims.width
                         h = round intState.loc.dims.height
                     in C.collage w h [render intState.diagram]

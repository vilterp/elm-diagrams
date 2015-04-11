module Diagrams.Interact where

{-| Abstractions for making diagrams which change as a function of the mouse.

# Function Types
@docs RenderFunc, UpdateFunc, InteractUpdateFunc

# Interaction
@docs InteractionState, initInteractionState, interactFold, makeFoldUpdate

# Mouse Event Processing
@docs processMouseEvent

# Mouse State
@docs MouseState, initMouseState
-}

import Signal as S
import Window
import Mouse

import List as L
import Maybe as M
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

-- TODO(perf): keep both unzipped for perforance?
type alias MouseState t a =
    { isDown : Bool
    , overTree : Maybe (PickTree t a)
    , tagPathOnMouseDown : Maybe (List t)
    }

initMouseState = { isDown = False, overTree = Nothing, tagPathOnMouseDown = Nothing }

type alias InteractionState m t a =
    { mouseState : MouseState t a
    , diagram : Diagram t a
    , modelState : m
    }

type alias RenderFunc m t a = m -> Diagram t a
type alias UpdateFunc m a = a -> m -> m
type alias InteractUpdateFunc m t a = (CollageLocation, PrimMouseEvent) -> InteractionState m t a -> InteractionState m t a

{-| Top-level interface to this module. Given
- how to update the state (type `m`) given an action (type `a`),
- how to render a diagram given the state,
- and how to compute the location of the collage on screen from the window dimensions,
Return a signal of diagrams.
-}
interactFold : UpdateFunc m a -> RenderFunc m t a -> CollageLocFunc -> m -> Signal (Diagram t a)
interactFold updateF renderF collageLocF initModel =
    let states = S.foldp (makeFoldUpdate updateF renderF)
                         (initInteractState renderF initModel)
                         (makeUpdateStream collageLocF)
    in S.map .diagram states

makeFoldUpdate : UpdateFunc m a -> RenderFunc m t a -> InteractUpdateFunc m t a
makeFoldUpdate updateF renderF =
    \(loc, evt) intState ->
        let (newMS, actions) = processMouseEvent intState.diagram intState.mouseState evt
            d = Debug.log "actions" actions
            newModel = L.foldr updateF intState.modelState actions
            newDiagram = renderF newModel
        in { mouseState = newMS
           , diagram = newDiagram
           , modelState = newModel
           }

initInteractState : RenderFunc m t a -> m -> InteractionState m t a
initInteractState render model =
    { mouseState = initMouseState
    , modelState = model
    , diagram = render model
    }

-- BUG: no initial pick path

-- TODO: fix these docs vv
{-| Given diagram with mouse state (`MouseDiagram`), mouse event, and dimensions of collage, return
new `MouseDiagram` with list of actions triggered by this mouse event. -}
processMouseEvent : Diagram t a -> MouseState t a -> PrimMouseEvent -> (MouseState t a, List a)
processMouseEvent diagram mouseState (evt, mousePos) =
    let overTree = Debug.watch "OT" <| pick diagram mousePos -- need to pick every time because actions may have changed
        overPickedTags = preorderTraverse overTree
        overTags = L.map .tag overPickedTags
    in case evt of
         MouseMoveEvt -> let oldOverPickedTags = preorderTraverse mouseState.overTree -- TODO: save this; don't do it twice
                             oldOverTags = L.map .tag oldOverPickedTags
                             enters = L.filterMap (getHandler .mouseEnter) <|
                                L.filter (\pTag -> not <| L.member pTag.tag oldOverTags) overPickedTags
                             leaves = L.filterMap (getHandler .mouseLeave) <|
                                L.filter (\pTag -> not <| L.member pTag.tag overTags) oldOverPickedTags
                             moves = L.filterMap (getHandler .mouseMove) <|
                                L.filter (\pTag -> L.member pTag.tag oldOverTags) overPickedTags
                         in ( { mouseState | overTree <- overTree }
                            , applyActions <| enters ++ leaves ++ moves
                            )
         MouseDownEvt -> ( { mouseState | isDown <- True
                                        , tagPathOnMouseDown <- Just overTags
                                        , overTree <- overTree }
                         , applyActions <| L.filterMap (getHandler .mouseDown) overPickedTags
                         )
         MouseUpEvt -> let mouseUps = L.filterMap (getHandler .mouseUp) overPickedTags
                           --a = Debug.log "---------------" ()
                           --b = Debug.log "op:" overPath
                           --c = Debug.log "ppomd:" mouseState.tagPathOnMouseDown
                           --d = Debug.log "match" (b == (M.withDefault [] c))
                           -- TODO: filter for ones that have same pick path on mouse down as now (?)
                           clicks = if overTags == M.withDefault [] mouseState.tagPathOnMouseDown
                                    then L.filterMap (getHandler .click) overPickedTags
                                    else []
                       in ( { mouseState | isDown <- False
                                         , tagPathOnMouseDown <- Nothing
                                         , overTree <- overTree }
                          , applyActions <| clicks ++ mouseUps
                          )

-- helpers for processMouseEvent

preorderTraverse : Maybe (PickTree t a) -> List (PickedTag t a)
preorderTraverse maybeTree =
    let recurse tree = case tree of
          PickLeaf -> []
          PickTag {tag, offset, actionSet, child} ->
              (recurse child) ++ [{ offset = offset, actionSet = actionSet, tag = tag }]
          PickLayers layers -> L.concatMap recurse layers
    in case maybeTree of
      Just tree -> recurse tree
      Nothing -> []

getHandler : (ActionSet t a -> Maybe (EventToAction t a))
                   -> PickedTag t a -> Maybe (PickedTag t a, EventToAction t a)
getHandler getMember pTag =
    case getMember pTag.actionSet of
      Just e2a -> Just (pTag, e2a)
      Nothing -> Nothing

applyActions : List (PickedTag t a, EventToAction t a) -> List a
applyActions pickedTags = 
    mapWithEarlyStop (\(pTag, e2a) -> e2a <| MouseEvent { offset = pTag.offset })
                     pickedTags

{-| Like map, but stops if the second element of the function result is True. -}
mapWithEarlyStop : (a -> (b, Bool)) -> List a -> List b
mapWithEarlyStop f l =
    case l of
      [] -> []
      (x::xs) -> case f x of
                   (y, True) -> [y]
                   (y, False) -> y :: (mapWithEarlyStop f xs)

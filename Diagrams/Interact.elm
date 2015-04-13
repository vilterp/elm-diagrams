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

type alias MouseState t a =
    { isDown : Bool
    , overPickedTags : List (PickedTag t a)
    , overPathsOnMouseDown : Maybe (List (List t))
    }

initMouseState = { isDown = False, overPickedTags = [], overPathsOnMouseDown = Nothing }

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
        overPaths = tagPaths overPickedTags
        oldOverPickedTags = mouseState.overPickedTags
        oldOverPaths = tagPaths oldOverPickedTags
    in case evt of
         MouseMoveEvt -> let enters = L.filterMap (getHandler .mouseEnter) <|
                                L.filter (\pTag -> not <| L.member (tagPath pTag) oldOverPaths) overPickedTags
                             leaves = L.filterMap (getHandler .mouseLeave) <|
                                L.filter (\pTag -> not <| L.member (tagPath pTag) overPaths) oldOverPickedTags
                             moves = L.filterMap (getHandler .mouseMove) <|
                                L.filter (\pTag -> L.member (tagPath pTag) oldOverPaths) overPickedTags
                         in ( { mouseState | overPickedTags <- overPickedTags }
                            , applyActions <| enters ++ leaves ++ moves
                            )
         MouseDownEvt -> ( { mouseState | isDown <- True
                                        , overPathsOnMouseDown <- Just overPaths
                                        , overPickedTags <- overPickedTags }
                         , applyActions <| L.filterMap (getHandler .mouseDown) overPickedTags
                         )
         MouseUpEvt -> let mouseUps = L.filterMap (getHandler .mouseUp) overPickedTags
                           --a = Debug.log "---------------" ()
                           --b = Debug.log "op:" overPath
                           --c = Debug.log "ppomd:" mouseState.overPathsOnMouseDown
                           --d = Debug.log "match" (b == (M.withDefault [] c))
                           -- TODO: filter for ones that have same pick path on mouse down as now (?)
                           clicks = if overPaths == M.withDefault [] mouseState.overPathsOnMouseDown
                                    then L.filterMap (getHandler .click) overPickedTags
                                    else []
                       in ( { mouseState | isDown <- False
                                         , overPathsOnMouseDown <- Nothing
                                         , overPickedTags <- overPickedTags }
                          , applyActions <| clicks ++ mouseUps
                          )

-- helpers for processMouseEvent

type alias PickedTag t a = { actionSet : ActionSet t a
                           , offset : Point
                           , tag : t
                           , path : PickPath t
                           }

tagPaths : List (PickedTag t a) -> List (List t)
tagPaths pTags =
    L.map tagPath pTags

tagPath : PickedTag t a -> List t
tagPath pTag = L.map .tag pTag.path

preorderTraverse : Maybe (PickTree t a) -> List (PickedTag t a)
preorderTraverse maybeTree =
    let recurse path tree =
          case tree of
            PickLeaf -> []
            PickTag {tag, offset, actionSet, child} ->
                let to = {tag=tag, offset=offset}
                in (recurse ({tag=tag, offset=offset}::path) child)
                      ++ [{ offset = offset, actionSet = actionSet, tag = tag, path = to::path }]
            PickLayers layers -> L.concatMap (recurse path) layers
    in case maybeTree of
      Just tree -> recurse [] tree
      Nothing -> []

getHandler : (ActionSet t a -> Maybe (EventToAction t a))
                   -> PickedTag t a -> Maybe (PickedTag t a, EventToAction t a)
getHandler getMember pTag =
    case getMember pTag.actionSet of
      Just e2a -> Just (pTag, e2a)
      Nothing -> Nothing

applyActions : List (PickedTag t a, EventToAction t a) -> List a
applyActions pickedTags = 
    mapWithEarlyStop (\(pTag, e2a) -> e2a <| Debug.log "ME" <| MouseEvent { offset = pTag.offset, path = pTag.path })
                     pickedTags

{-| Like map, but stops if the second element of the function result is True. -}
mapWithEarlyStop : (a -> (b, Bool)) -> List a -> List b
mapWithEarlyStop f l =
    case l of
      [] -> []
      (x::xs) -> case f x of
                   (y, True) -> [y]
                   (y, False) -> y :: (mapWithEarlyStop f xs)

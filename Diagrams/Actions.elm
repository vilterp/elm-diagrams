module Diagrams.Actions where

{-| A type for attaching mouse actions to diagrams.

@docs ActionSet, EventToAction, emptyActionSet, collageMousePos
-}

import List as L
import Debug

import Diagrams.Geom exposing (..)

-- TODO: tagging function easier than `tagWithAction tag { emptyActionSet | click <- Just ... } dia`?
-- like `clickable tag func dia` or something
-- or list of attributes like html

-- TODO: odd place for these to live
{-| (Tag, Coordinates) pairs from bottom of tree to top; result of
calling `pick` (see below). -}
type PickTree t a
    = PickLayers (List (PickTree t a))
    | PickLeaf
    | PickTag { tag : t
              , offset : Point
              , actionSet : ActionSet t a
              , child : PickTree t a
              }

type alias PickPath t = List (PickPathElem t)
type alias PickPathElem t = { tag : t, offset : Point }

{-| Path: list of (tag, offset) from bottom of tree to top.
Offset: offset at lowest level in tree. -}
-- TODO: a type variable not needed
type MouseEvent t a = MouseEvent { offset : Point
                                 , path : PickPath t
                                 }

{-| Given an event, return (a) an action resulting from that event, and (b) whether to stop this
mouse event from "bubbling up" to handlers higher up the tree. -}
type alias EventToAction t a = MouseEvent t a -> (a, Bool)
type alias ActionSet t a =
    { click : Maybe (EventToAction t a)
    , mouseEnter : Maybe (EventToAction t a)
    , mouseLeave : Maybe (EventToAction t a)
    , mouseMove : Maybe (EventToAction t a)
    , mouseDown : Maybe (EventToAction t a)
    , mouseUp : Maybe (EventToAction t a)
    }

emptyActionSet =
    { click = Nothing
    , mouseEnter = Nothing
    , mouseLeave = Nothing
    , mouseMove = Nothing
    , mouseDown = Nothing
    , mouseUp = Nothing
    }

keepBubbling : (MouseEvent t a -> a) -> EventToAction t a
keepBubbling f = \evt -> (f evt, False)

stopBubbling : (MouseEvent t a -> a) -> EventToAction t a
stopBubbling f = \evt -> (f evt, True)

-- why is this not in the stdlib?
-- empty list => incomplete pattern match
last : List a -> a
last l = case l of
           [] -> Debug.crash "last expects a non-empty list"
           [x] -> x
           (x::xs) -> last xs

mousePosAtPath : MouseEvent t a -> List t -> Maybe Point
mousePosAtPath (MouseEvent evt) tagPath =
    let a = Debug.log "pp, tp" (evt.path, tagPath)
        recurse : PickPath t -> List t -> Maybe Point
        recurse pp tp =
          case (pp, tp) of
            ({tag, offset}::evtTags, [wantedTag]) ->
                if tag == wantedTag then Just offset else Nothing
            ({tag, offset}::evtTags, wantedTag::tags) ->
                if tag == wantedTag then recurse evtTags tags else Nothing
            _ -> Nothing
    in recurse (L.reverse evt.path) tagPath

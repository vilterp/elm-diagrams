module Diagrams.Actions where

{-| A type for attaching mouse actions to diagrams.

@docs ActionSet, EventToAction, emptyActionSet
-}

import Diagrams.Geom (..)

-- TODO: tagging function easier than `tagWithAction tag { emptyActionSet | click <- Just ... } dia`?
-- like `clickable tag func dia` or something
-- or list of attributes like html

-- TODO: odd place for these to live
{-| (Tag, Coordinates) pairs from bottom of tree to top; result of
calling `pick` (see below). -}
type alias PickPath t a = List (PickPathElem t a)
type alias PickPathElem t a = { tag : t
                              , actionSet : ActionSet t a
                              , offset : Point
                              }
type MouseEvent t a = MouseEvent { offset : Point
                                 , pickPath : PickPath t a
                                 }

type alias EventToAction t a = MouseEvent t a -> a
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

module Diagrams.Actions where

import Diagrams.Geom (..)

-- TODO: tagging function easier than `tagWithAction tag { emptyActionSet | click <- Just ... } dia`?
-- like `clickable tag func dia` or something
-- or list of attributes like html

type alias EventToAction a = Point -> a
type alias ActionSet a =
    { click : Maybe (EventToAction a)
    , mouseEnter : Maybe (EventToAction a)
    , mouseLeave : Maybe (EventToAction a)
    , mouseMove : Maybe (EventToAction a)
    , mouseDown : Maybe (EventToAction a)
    , mouseUp : Maybe (EventToAction a)
    }
emptyActionSet =
    { click = Nothing
    , mouseEnter = Nothing
    , mouseLeave = Nothing
    , mouseMove = Nothing
    , mouseDown = Nothing
    , mouseUp = Nothing
    }

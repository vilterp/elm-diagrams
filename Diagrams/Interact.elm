module Diagrams.Interact where

import Signal

-- TODO: how to import from diagrams?
type alias Point = (Float, Float)

type alias Actions a =
    { click : Maybe a
    , mouseEnter : Maybe a
    , mouseLeave : Maybe a
    , mouseDown : Maybe a
    , mouseUp : Maybe a
    }

-- TODO: use extensible records?
type alias MouseEvent = (MouseAction, Point)
type MouseAction = MouseUp
                 | MouseDown
                 | MouseMove

-- TODO: how to import from diagrams...
type MouseState a = { lastAction : MouseAction, overPath : PickPath a }

-- TODO: how to import from diagrams...
type MouseDiagram a = { mouseState : MouseState a, diagram : Diagram a }

process : MouseDiagram a -> MouseEvent -> (MouseDiagram, [a])
process = ...
-- TODO: make easy to drop into foldp

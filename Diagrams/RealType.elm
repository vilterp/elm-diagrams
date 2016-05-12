module Diagrams.RealType exposing (..)

import Element as E
import Collage as C
import Text as T

import Diagrams.FillStroke exposing (..)
import Diagrams.Geom exposing (..)
import Diagrams.Actions exposing (..)

type Diagram t a
    -- primitives
    = Circle Float FillStroke
    | Rect Float Float FillStroke
    | Polygon (List Point) FillStroke
    | Path (List Point) C.LineStyle
    | Text String T.Style E.Element
    -- transformation
    | TransformD Transform (Diagram t a)
    -- group
    | Group (List (Diagram t a))
    -- tag
    | Tag t (ActionSet t a) (Diagram t a)

module Diagrams.Type (Diagram) where

{-|
@docs Diagram
-}

import Diagrams.RealType

{-| A diagram. See `Diagrams.Core` for constructor functions. -}
type alias Diagram t a =
    Diagrams.RealType.Diagram t a

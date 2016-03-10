module Diagrams.Align where

{-|
# Relative Positioning
@docs beside, above, atop, hcat, vcat, zcat

# Moving the origin
@docs alignLeft, alignRight, alignTop, alignBottom, alignCenter, hAlign, vAlign

# Aligning lists of diagrams
@docs HAlign, VAlign, hcatA, vcatA
-}

import List as L

import Diagrams.Core exposing (..)
import Diagrams.Geom exposing (..)
import Diagrams.Envelope exposing (..)
import Diagrams.Type as T
import Diagrams.RealType exposing (..)

-- Relative Positioning

{-| Given two diagrams a and b, place b to the right of a, such that their origins
are on a horizontal line and their envelopes touch. The origin of the new diagram
is the origin of a. -}
beside : T.Diagram t a -> T.Diagram t a -> T.Diagram t a
beside a b =
  let xTrans = (envelope Right a) + (envelope Left b)
  in Group [a, TransformD (Translate xTrans 0) b]

{-| Given two diagrams a and b, place b to the right of a, such that their origins
are on a horizontal line and their envelopes touch. The origin of the new diagram
is the origin of a. -}
above : T.Diagram t a -> T.Diagram t a -> T.Diagram t a
above a b =
  let yTrans = (envelope Down a) + (envelope Up b)
  in Group [a, TransformD (Translate 0 -yTrans) b]

{-| Given two diagrams a and b, stack a on top of b in the "out of page" axis,
so a occludes b. -}
atop : T.Diagram t a -> T.Diagram t a -> T.Diagram t a
atop a b = Group [a, b]

{-| Place a list of Diagrams next to each other, such that
their origins are along a horizontal line. The first element in the list will
be on the left; the last on the right. -}
hcat : List (T.Diagram t a) -> T.Diagram t a
hcat = L.foldr beside empty

{-| Place a list of Diagrams next to each other, such that
their origins are along a vertical line. The first element in the list will
be on the top; the last on the bottom. -}
vcat : List (T.Diagram t a) -> T.Diagram t a
vcat = L.foldr above empty

{-| Place a list of diagrams on top of each other, with their origin points
stacked on the "out of page" axis. The first diagram in the list is on top.
This is the same as the `group`. -}
zcat : List (T.Diagram t a) -> T.Diagram t a
zcat = Group -- lol

-- Alignment

{-| Translate a diagram such that the origin is on the left edge of the bounding box -}
alignLeft : T.Diagram t a -> T.Diagram t a
alignLeft dia =
  let leftEnv = envelope Left dia
  in moveX leftEnv dia

{-| Translate a diagram such that the origin is on the right edge of the bounding box -}
alignRight : T.Diagram t a -> T.Diagram t a
alignRight dia =
  let rightEnv = envelope Right dia
  in moveX -rightEnv dia


{-| Translate a diagram such that the origin is on the top edge of the bounding box -}
alignTop : T.Diagram t a -> T.Diagram t a
alignTop dia =
  let upEnv = envelope Up dia
  in moveY -upEnv dia

{-| Translate a diagram such that the origin is on the bottom edge of the bounding box -}
alignBottom : T.Diagram t a -> T.Diagram t a
alignBottom dia =
  let downEnv = envelope Down dia
  in moveY downEnv dia

{-| Translate a diagram such that the envelope in all directions is equal -}
alignCenter : T.Diagram t a -> T.Diagram t a
alignCenter dia =
  let
    left = envelope Left dia
    right = envelope Right dia
    xTrans = (right - left)/2
    up = envelope Up dia
    down = envelope Down dia
    yTrans = (down-up)/2
  in
    move (-xTrans, yTrans) dia

{-|-}
type HAlign = LeftA | RightA

{-|-}
type VAlign = TopA | BottomA

{-| Choose alignLeft or alignRight based on the given `HAlign`. -}
hAlign : HAlign -> T.Diagram t a -> T.Diagram t a
hAlign ha dia =
  case ha of
    LeftA -> alignLeft dia
    RightA -> alignRight dia

{-| Choose alignTop or alignBottom based on the given `VAlign`. -}
vAlign : VAlign -> T.Diagram t a -> T.Diagram t a
vAlign va dia =
  case va of
    TopA -> alignTop dia
    BottomA -> alignBottom dia

{-| Align a list of diagrams along a horizontal line according to the
given `VAlign`. -}
hcatA : VAlign -> List (T.Diagram t a) -> T.Diagram t a
hcatA va dias = hcat <| L.map (vAlign va) dias

{-| Align a list of diagrams along a vertical line according to the
given `HAlign`. -}
vcatA : HAlign -> List (T.Diagram t a) -> T.Diagram t a
vcatA ha dias = vcat <| L.map (hAlign ha) dias

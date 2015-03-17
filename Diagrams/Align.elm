module Diagrams.Align where

import List as L

import Diagrams.Core (..)
import Diagrams.Geom (..)
import Diagrams.Envelope (..)

{-| Stack diagrams vertically (as with `vcat`), such that their left edges align.
The origin of the resulting diagram is the origin of the first diagram. -}
alignLeft : List (Diagram t a) -> Diagram t a
alignLeft dias = let leftEnvelopes = L.map (envelope Left) dias
                     maxLE = L.maximum leftEnvelopes
                     moved = L.map2 (\dia le -> moveX -(maxLE - le) dia) dias leftEnvelopes
                 in vcat moved

{-| translate a diagram such that the envelope in all directions is equal -}
alignCenter : Diagram t a -> Diagram t a
alignCenter dia = let left = envelope Left dia
                      right = envelope Right dia
                      xTrans = (right - left)/2
                      up = envelope Up dia
                      down = envelope Down dia
                      yTrans = (down-up)/2
                  in move (-xTrans, yTrans) dia

{-| Given two diagrams a and b, place b to the right of a, such that their origins
are on a horizontal line and their envelopes touch. The origin of the new diagram
is the origin of a. -}
beside : Diagram t a -> Diagram t a -> Diagram t a
beside a b = let xTrans = (envelope Right a) + (envelope Left b)
             in Group [a, TransformD (Translate xTrans 0) b]

{-| Given two diagrams a and b, place b to the right of a, such that their origins
are on a horizontal line and their envelopes touch. The origin of the new diagram
is the origin of a. -}
above : Diagram t a -> Diagram t a -> Diagram t a
above a b = let yTrans = (envelope Down a) + (envelope Up b)
              in Group [a, TransformD (Translate 0 -yTrans) b]

{-| Given two diagrams a and b, stack a on top of b in the "out of page" axis,
so a occludes b. -}
atop : Diagram t a -> Diagram t a -> Diagram t a
atop a b = Group [a, b]

{-| Place a list of Diagrams next to each other, such that
their origins are along a horizontal line. The first element in the list will
be on the left; the last on the right. -}
hcat : List (Diagram t a) -> Diagram t a
hcat = L.foldr beside empty

{-| Place a list of Diagrams next to each other, such that
their origins are along a vertical line. The first element in the list will
be on the top; the last on the bottom. -}
vcat : List (Diagram t a) -> Diagram t a
vcat = L.foldr above empty

{-| Place a list of diagrams on top of each other, with their origin points
stacked on the "out of page" axis. The first diagram in the list is on top.
This is the same as the `group`. -}
zcat : List (Diagram t a) -> Diagram t a
zcat = Group -- lol

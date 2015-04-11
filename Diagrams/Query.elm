module Diagrams.Query where

{-| Retreive information about laid-out diagrams.

@docs pick, TagPath, getCoords
-}

import List as L
import List ((::))
import Maybe as M
import Graphics.Element as E

import Diagrams.Core (..)
import Diagrams.Geom (..)
import Diagrams.Actions (..)
import Diagrams.FillStroke (..)

{-| Given a Diagram t and a point (e.g. of the mouse), return the list of Tag nodes between
the diagram root and the leaf node the point is within (a primitive visual element), along
with the point's offset from the local origin at each level. If the mouse is not over a leaf
node, return `[]`.

Returns a `PickPath`, which is a list of (Tag, Coordinates) pairs ordered
from the leaf node to the root. The second element in each pair is the given
point in the tag's coordinate space -- the point which, if transformed by every
Transform node from that tag node to the root,  would be the point initially given to `pick`.

    pick myDiagram myPoint
    => [(<tag nearest myDiagram leaf>, <myPoint in leaf coordinate space>),
       , ...
       , (<tag nearest myDiagram root>, <myPoint in root coordinate space>)]

-}
pick : Diagram t a -> Point -> Maybe (PickTree t a)
pick diag point =
    let handleBox w h borderWidth =
          let (x, y) = point
              w2 = w/2 + borderWidth
              h2 = h/2 + borderWidth
          in if x < w2 && x > -w2 && y < h2 && y > -h2
             then Just PickLeaf
             else Nothing
    in case diag of
         Circle r fs -> if magnitude point <= r + (halfStrokeWidth fs) then Just PickLeaf else Nothing
         Path pts _ _ -> Nothing -- TODO implement picking for paths
         Rect w h fs -> handleBox w h (halfStrokeWidth fs)
         Text _ _ te -> handleBox (toFloat <| E.widthOf te) (toFloat <| E.heightOf te) 0
         Group dias -> case L.filterMap (\d -> pick d point) dias of
                          [] -> Nothing
                          [x] -> Just x
                          xs -> Just <| PickLayers xs
         Tag t acts diagram -> 
            pick diagram point |> M.map (\res -> PickTag { tag = t
                                                        , actionSet = acts
                                                        , offset = point
                                                        , child = res
                                                        })
         TransformD trans diagram -> pick diagram (applyTrans (invertTrans trans) point)

-- like M.oneOf, for lists...
firstNonempty : List (List a) -> List a
firstNonempty l = case l of
                    [] -> []
                    []::xs -> firstNonempty xs
                    x::xs -> x

type alias TagPath a = List a

{-| Try to find a subDiagram t at the given tag path. If it is found,
return the coordinates of its origin relative to the origin of this diagram. If
It isn't found, return `Nothing`. -}
getCoords : Diagram t a -> TagPath t -> Maybe Point
getCoords dia path = getCoords' dia path (0, 0)

getCoords' : Diagram t a -> TagPath t -> Point -> Maybe Point
getCoords' diag path start = 
    case path of
      [] -> Just start
      (x::xs) -> 
        case diag of
          Tag t _ dia -> if x == t then getCoords' dia xs start else Nothing
          Group dias -> M.oneOf <| L.map (\d -> getCoords' d path start) dias
          TransformD trans dia -> getCoords' dia path (applyTrans trans start)
          _ -> Nothing

module Diagrams.Query exposing ( PickTree(..), pick, getCoords, TagPath )

{-| Retreive information about laid-out diagrams.

# Pick
@docs pick, PickTree

# Get Coordinates
@docs getCoords, TagPath
-}

import List as L
import Maybe as M
import Element as E

import Diagrams.Core exposing (..)
import Diagrams.Geom exposing (..)
import Diagrams.Actions exposing (..)
import Diagrams.FillStroke exposing (..)
import Diagrams.Type as T
import Diagrams.RealType exposing (..)

{-| Result of `pick`: tree representing the subtree of the Diagram the
given point is over.

- Leaf nodes mean the mouse is over a primitive shape (rect, text, etc)
- Tag nodes mean the child of this pickTree is under that tag.
- Layers mean that the mouse is over two overlapping diagrams. The
  pick trees for these diagrams are given in a list starting with the
  one on top. -}
type PickTree t a
    = PickLayers (List (PickTree t a))
    | PickLeaf
    | PickTag { tag : t
              , offset : Point
              , actionSet : ActionSet t a
              , child : PickTree t a
              }

{-| Given a diagram and a point (e.g. of the mouse), return a `PickTree`, which represents
what subtree of the diagram that point is currently over. -}
pick : T.Diagram t a -> Point -> Maybe (PickTree t a)
pick diag point =
  let
    handleBox w h borderWidth =
      let
        (x, y) = point
        w2 = w/2 + borderWidth
        h2 = h/2 + borderWidth
      in
        if x < w2 && x > -w2 && y < h2 && y > -h2
        then Just PickLeaf
        else Nothing
  in
    case diag of
      Circle r fs ->
        if magnitude point <= r + (halfStrokeWidth fs) then
          Just PickLeaf
        else
          Nothing

      Path pts _ ->
        Nothing -- TODO implement picking for paths

      Polygon _ _ ->
        Nothing -- TODO: picking for polygons

      Rect w h fs ->
        handleBox w h (halfStrokeWidth fs)

      Text _ _ te ->
        handleBox (toFloat <| E.widthOf te) (toFloat <| E.heightOf te) 0

      Group dias ->
        case L.filterMap (\d -> pick d point) dias of
          [] -> Nothing
          [x] -> Just x
          xs -> Just <| PickLayers xs

      Tag t acts diagram -> 
         pick diagram point
          |> M.map (\res ->
              PickTag
                { tag = t
                , actionSet = acts
                , offset = point
                , child = res
                })

      TransformD trans diagram ->
        pick diagram (applyTrans (invertTrans trans) point)

-- like M.oneOf, for lists...
firstNonempty : List (List a) -> List a
firstNonempty l =
  case l of
    [] -> []
    []::xs -> firstNonempty xs
    x::xs -> x

{-|-}
type alias TagPath a = List a

{-| Try to find a subDiagram t at the given tag path. If it is found,
return `Just` the coordinates of its origin relative to the origin of this diagram.
If it isn't found, return `Nothing`. -}
getCoords : TagPath t -> T.Diagram t a -> Maybe Point
getCoords path dia =
    let
      recurse diag path start = 
        case path of
          [] ->
            Just start

          (x::xs) -> 
            case diag of
              Tag t _ dia ->
                  if x == t
                  then recurse dia xs start
                  else Nothing
              Group dias ->
                  M.oneOf <| L.map (\d -> recurse d path start) dias
              TransformD trans dia ->
                  recurse dia path (applyTrans trans start)
              _ -> Nothing
    in
      recurse dia path (0, 0)

module Diagrams.Envelope exposing ( envelope, width, height, boundingBox, dims )
-- where

{-|
@docs envelope, width, height, boundingBox, dims
-}

import List as L
import Element as E
import Maybe as M

import Diagrams.Core exposing (..)
import Diagrams.Geom exposing (..)
import Diagrams.FillStroke exposing (..)
import Diagrams.Type as T
import Diagrams.RealType exposing (..)

{-| Given a diagram and a Direction, return the distance in that direction from the origin
to the closest line perpendicular to that direction which doesn't intersect the content of
the diagram. See the [Haskell diagrams docs][hd] for a visual explanation.

 [hd]: http://projects.haskell.org/diagrams/doc/manual.html#envelopes-and-local-vector-spaces
-}
envelope : Direction -> T.Diagram t a -> Float
envelope dir dia =
  let
    handleBox w h borderWidth =
      let base = case dir of
                   Up -> h/2
                   Down -> h/2
                   Left -> w/2
                   Right -> w/2
      in base + borderWidth

    handlePath path =
      let
        xs = L.map fst path
        ys = L.map snd path
      in
        case dir of
          Left -> -(def0 <| L.minimum xs)
          Right -> def0 <| L.maximum xs
          Up -> def0 <| L.maximum ys
          Down -> -(def0 <| L.minimum ys)
  in
    case dia of
      Tag _ _ dia' ->
        envelope dir dia'

      Group dias ->
        case dias of -- TODO: cache
          [] -> 0
          _ -> def0 <| L.maximum <| L.map (envelope dir) dias

      TransformD (Scale s) diag ->
        s * (envelope dir diag)

      TransformD (Rotate r) rotDia ->
          case rotDia of
            -- TODO: DRY
            Path points fs ->
              let
                newPoints =
                  L.map (applyTrans <| Rotate r) points
              in
                envelope dir <| Path newPoints fs

            Polygon points ls ->
              let
                newPoints =
                  L.map (applyTrans <| Rotate r) points
              in
                envelope dir <| Polygon newPoints ls

            Circle _ _ ->
              envelope dir rotDia

            _ ->
              Debug.crash "TODO"
            -- TODO: handleBox for rect, text
      TransformD (Translate tx ty) diag ->
        let
          env =
            envelope dir diag
        in
          case dir of
            Up -> max 0 <| env + ty
            Down -> max 0 <| env - ty
            Right -> max 0 <| env + tx
            Left -> max 0 <| env - tx

      Text str ts te ->
        handleBox (toFloat <| E.widthOf te) (toFloat <| E.heightOf te) 0

      Path points fs ->
        handlePath points

      Polygon points fs ->
        handlePath points

      Rect w h fs ->
        handleBox w h (halfStrokeWidth fs)

      Circle r fs ->
        r + (halfStrokeWidth fs)

{-|-}
width : T.Diagram t a -> Float
width d =
  (envelope Left d) + (envelope Right d)

{-|-}
height : T.Diagram t a -> Float
height d =
  (envelope Up d) + (envelope Down d)

{-| Box formed by taking the envelope in all directions. -}
boundingBox : T.Diagram t a -> BBox
boundingBox dia =
  { up = envelope Up dia
  , down = envelope Down dia
  , left = envelope Left dia
  , right = envelope Right dia
  }

{-|-}
dims : T.Diagram t a -> Dims
dims dia =
  let
    bbox =
      boundingBox dia
  in
    { width = bbox.left + bbox.right
    , height = bbox.up + bbox.down
    }

def0 : Maybe number -> number
def0 m =
  M.withDefault 0 m

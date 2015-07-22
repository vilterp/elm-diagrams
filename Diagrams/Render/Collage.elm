module Diagrams.Render.Collage where

import Text as T
import List as L
import Graphics.Collage as C

import Diagrams.Geom exposing (..)
import Diagrams.FillStroke exposing (..)
import Diagrams.Core exposing (..)

{-|-}
render : Diagram t a -> C.Form
render d =
  let
    handleFS fs pathType shape =
      let
        filled =
          case fs.fill of
            Just fillStyle ->
              case fillStyle of
                Solid color -> [C.filled color shape]
                Texture src -> [C.textured src shape]
                Grad grad -> [C.gradient grad shape]
            Nothing -> []

        stroked =
          case fs.stroke of
            Just strokeStyle ->
              case pathType of
                ClosedP -> [C.outlined strokeStyle shape]
                OpenP -> [C.traced strokeStyle shape]
            Nothing -> []
      in
        C.group <| stroked ++ filled
 in
  case d of
    Tag _ _ dia -> render dia
    Group dias -> C.group <| L.map render <| L.reverse dias -- TODO: this seems semantically right; don't want to
                                                            -- have to reverse tho
    TransformD (Scale s) dia -> C.scale s <| render dia
    TransformD (Rotate r) dia -> C.rotate r <| render dia
    TransformD (Translate x y) dia -> C.move (x, y) <| render dia
    Text str ts te -> C.text <| T.style ts <| T.fromString str
    Path path fs ty -> handleFS fs ty path
    Rect w h fs -> handleFS fs ClosedP <| C.rect w h
    Circle r fs -> handleFS fs ClosedP <| C.circle r

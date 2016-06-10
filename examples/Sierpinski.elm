module Test exposing (..)
-- where

import Diagrams.Core exposing (..)
import Diagrams.Type exposing (..)
import Diagrams.Align exposing (..)
import Diagrams.FillStroke exposing (..)
import Diagrams.FullWindow exposing (..)
import Diagrams.Debug exposing (..)
import Color


sierpinski : Int -> Float -> Diagram t a
sierpinski n sl =
  case n of
    0 ->
      eqTriangle sl (justSolidFill Color.blue)

    _ ->
      let
        smaller =
          sierpinski (n-1) sl
      in
        smaller `above`
        (alignCenter <| smaller `beside` smaller)
        |> alignCenter


-- not spinning
main =
  sierpinski 3 20
  |> fullWindowMain


-- fullWindowMain <| showOrigin <| showBBox <| alignCenter <| sierpinski 3 20

-- spinning
--main = let sier = sierpinski 5 10
--       in Signal.map2 (\wh ang -> fullWindowView wh <| showOrigin <| rotate ang sier) Window.dimensions angle

--framesPast = Signal.foldp (\_ st -> st + 1) 0 (Time.fps 30)
--framesPerRotation = 100
--angle = Signal.map (\past -> ((toFloat <| past % framesPerRotation) / framesPerRotation) * (2*pi)) framesPast

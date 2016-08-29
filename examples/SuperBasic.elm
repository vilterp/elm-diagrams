module SuperBasic exposing (..)
-- where

import Element
import Collage
import Window
import Html exposing (Html)
import Html.App as App

import Color
import Debug
import Text as T

-- whoooo all the moduless
import Diagrams.Core as Diagrams exposing (..)
import Diagrams.Type exposing (..)
import Diagrams.Query exposing (..)
import Diagrams.Svg
--import Diagrams.Interact exposing (..)
--import Diagrams.Wiring exposing (..)
import Diagrams.Geom exposing (..)
import Diagrams.Debug exposing (..)
import Diagrams.Align exposing (..)
import Diagrams.Pad exposing (..)
import Diagrams.Actions exposing (..)
import Diagrams.FillStroke exposing (..)
import Diagrams.FullWindow exposing (..)


testDia =
  vcat
    [ Diagrams.circle 20 (justSolidFill Color.blue)
    , Diagrams.circle 20 (justSolidFill Color.orange)
    ]
  |> showOrigin
  |> showBBox


--testDia = 
--  rect 100 20 (justSolidFill Color.blue)
--  |> showBBox
--  |> showOrigin


type alias Model = ()


-- TODO: use window subscription
dims =
  { width = 800
  , height = 800
  }


main =
  testDia |> fullWindowShow

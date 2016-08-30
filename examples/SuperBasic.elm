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

import Diagrams.Geom exposing (..)
import Diagrams.Debug exposing (..)
import Diagrams.Align exposing (..)
import Diagrams.Pad exposing (..)
import Diagrams.Actions exposing (..)
import Diagrams.FillStroke exposing (..)
import Diagrams.FullWindow exposing (..)
import Diagrams.Actions exposing (..)


type Tag
  = Foo


type Msg
  = FooMsg


view : Int -> Diagram Tag Msg
view x =
  vcat
    [ Diagrams.circle 20 (justSolidFill Color.blue)
      |> tagWithActions Foo { emptyActionSet | click = Just (\evt -> ([FooMsg], True)) }
    , Diagrams.circle 20 (justSolidFill Color.orange)
    , Diagrams.text T.defaultStyle (toString x)
    ]
  |> showOrigin
  |> showBBox


type alias Model = ()


-- TODO: use window subscription
dims =
  { width = 800
  , height = 800
  }


main =
  fullWindowProgram
    { view = view
    , update = \FooMsg x -> x + 1
    , model = 0
    }

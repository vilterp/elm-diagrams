module Basic exposing (..)
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
--import Diagrams.FullWindow exposing (..)


type Tag
  = RectOrange
  | RectBlue
  | Circ
  | Textt

type Action
  = ClickCirc Point
  | EnterOrange Point
  | LeaveOrange Point
  | MoveBlue Point


defLine = Collage.defaultLine


testDia : Diagram Tag Action
testDia =
  let
    aPath =
      path [(-50,-50), (30, 100)] Collage.defaultLine

    rectOrange =
      tagWithActions RectOrange
        { emptyActionSet | mouseEnter = Just <| keepBubbling <| (\(MouseEvent evt) -> [EnterOrange evt.offset])
                         , mouseLeave = Just <| keepBubbling <| (\(MouseEvent evt) -> [LeaveOrange evt.offset]) }
        --<| rect 50 70 (fillAndStroke (Solid Color.orange) { defLine | width = 20, cap = Collage.Padded })
        <| rect 50 70 (justFill (Solid Color.orange))

    rectBlue =
      tagWithActions RectBlue
        { emptyActionSet | mouseMove = Just <| keepBubbling <| (\(MouseEvent evt) -> [MoveBlue evt.offset]) }
        <| rect 70 50 (justSolidFill Color.blue)

    rects =
      vcat [ rectOrange , rectBlue ]

    circ =
      tagWithActions Circ
        { emptyActionSet | click = Just <| keepBubbling <| (\(MouseEvent evt) -> [ClickCirc evt.offset]) }
        <| circle 20 (fillAndStroke (Solid Color.yellow) { defLine | width = 2, cap = Collage.Padded })

    justText =
      text (let ds = T.defaultStyle in {ds | bold = True}) "Foo"

    someText =
      tag Textt <| background (justSolidFill Color.lightBlue) <| pad 5 <| justText

    stuff =
      circ `atop` (rectOrange `above` (rectBlue `beside` (circ `above` someText)))

    moreStuff =
      hcat <| List.intersperse circ (List.repeat 5 rectOrange)
  in
    --stuff `above` stuff `above` moreStuff
    (rectOrange `above` rectBlue)
    |> alignCenter
    --|> showBBox
    --|> showOrigin


type alias Model = ()


-- TODO: use window subscription
dims =
  { width = 800
  , height = 800
  }


main =
  App.beginnerProgram
    { model = ()
    , view = always (Diagrams.Svg.toHtml dims testDia)
    , update = (\_ m -> m)
    }

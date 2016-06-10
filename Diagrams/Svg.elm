module Diagrams.Svg exposing (..) -- where

import String

import Svg exposing (Svg)
import Html exposing (Html)
import Svg.Attributes as SvgA
import Color exposing (Color)

import List.Extra
import Collage

import Diagrams.Geom exposing (..)
import Diagrams.RealType as Type exposing (..)
import Diagrams.FillStroke exposing (..)


toSvg : Type.Diagram t a -> Svg b
toSvg d =
  let
    -- TODO: move these helpers out?
    getFillColor : FillStroke -> Maybe Color
    getFillColor fillStroke =
      fillStroke.fill
      `Maybe.andThen` (\fill ->
        case fill of
          Solid color ->
            Just color

          _ ->
            Debug.log
              "TODO: gradients, etc. probably just use Svg stuff"
              Nothing
      )
    
    getFillAttr : FillStroke -> List (Svg.Attribute a)
    getFillAttr fillStroke =
      fillStroke
      |> getFillColor
      |> Maybe.map (\color -> SvgA.fill (colorToCss color))
      |> maybeToList

    fromLineStyle : Collage.LineStyle -> List (Svg.Attribute a)
    fromLineStyle lineStyle =
      [ SvgA.stroke (colorToCss lineStyle.color)
      , SvgA.strokeWidth (toString lineStyle.width) -- cmon Svg lib; take a Float
      -- TODO: cap, join, dashing, dashOffset
      ]

    getStrokeAttrs : FillStroke -> List (Svg.Attribute a)
    getStrokeAttrs fillStroke =
      fillStroke.stroke
      |> Maybe.map fromLineStyle
      |> Maybe.withDefault []

    getFillStrokeAttrs : FillStroke -> List (Svg.Attribute a)
    getFillStrokeAttrs fs =
      (getFillAttr fs) ++ (getStrokeAttrs fs)

    pathToString : List Point -> String
    pathToString points =
      points
      |> List.map (\(x, y) -> toString x ++ "," ++ toString y)
      |> String.join ","
  in
    case d of
      Tag _ _ dia ->
        toSvg dia

      Group dias ->
        dias
        |> List.reverse
        |> List.map toSvg
        |> Svg.g []

      -- TODO: this seems semantically right; don't want to
      -- have to reverse tho
      TransformD (Scale s) dia ->
        dia
        |> toSvg
        |> List.Extra.singleton
        |> Svg.g []

      TransformD (Rotate r) dia ->
        dia
        |> toSvg
        |> List.Extra.singleton
        |> Svg.g []

      TransformD (Translate x y) dia ->
        dia
        |> toSvg
        |> List.Extra.singleton
        |> Svg.g [ SvgA.transform ("translate(" ++ toString x ++ " " ++ toString y ++ ")") ]

      Text str ts te ->
        -- TODO: styling  
        Svg.text' [] [Svg.text str]

      Path path ls ->
        Svg.polyline
          (List.concat
            [ [SvgA.points (pathToString path)]
            , fromLineStyle ls
            ])
          []

      Polygon points fs ->
        Svg.polygon
          (List.concat
            [ [SvgA.points (pathToString points)]
            , getFillStrokeAttrs fs
            ])
          []

      Rect w h fs ->
        Svg.rect
          (List.concat
            [ [ SvgA.width (toString w), SvgA.height (toString h) ]
            , getFillStrokeAttrs fs
            ])
          []

      Circle radius fs ->
        Svg.circle
          (List.concat
            [ [ SvgA.r (toString radius) ]
            , getFillStrokeAttrs fs
            ])
          []


toHtml : Dims -> Type.Diagram t a -> Html b
toHtml dims dia =
  let
    minX =
      dims.width / -2

    minY =
      dims.height / -2
  in
    dia
    |> toSvg
    |> List.Extra.singleton
    |> Svg.svg
        [ SvgA.width (toString dims.width)
        , SvgA.height (toString dims.height)
        , SvgA.viewBox
            (toString minX ++ " " 
              ++ toString minY ++ " "
              ++ toString dims.width ++ " "
              ++ toString dims.height)
        ]


-- why tf is this not in list-extra
maybeToList : Maybe a -> List a
maybeToList maybe =
  maybe
  |> Maybe.map List.Extra.singleton
  |> Maybe.withDefault []


-- this should also be in some library
colorToCss : Color -> String
colorToCss color =
  let
    rgb =
      Color.toRgb color
  in
    "rgba(" ++ toString rgb.red
      ++ "," ++ toString rgb.green
      ++ "," ++ toString rgb.blue
      ++ "," ++ toString rgb.alpha
      ++ ")"

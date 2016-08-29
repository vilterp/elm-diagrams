module Diagrams.Svg exposing (..) -- where

import String

import Svg exposing (Svg)
import Html exposing (Html)
import Svg.Attributes as SvgA
import Color exposing (Color)
import Element as E

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
      |> Maybe.withDefault (SvgA.fill "none")
      |> List.Extra.singleton

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
      |> List.map (\(x, y) -> toString x ++ "," ++ toString -y)
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
        |> Svg.g [ SvgA.transform ("rotate(" ++ toString (lerp (0, 360) (0, pi * 2) r) ++ ")") ]

      TransformD (Translate x y) dia ->
        dia
        |> toSvg
        |> List.Extra.singleton
        |> Svg.g [ SvgA.transform ("translate(" ++ toString x ++ " " ++ toString (-y) ++ ")") ]

      Text str ts te ->
        let
          negativeHalfToString x =
            (toFloat x) / 2
            |> toString
        in
          Svg.text'
            [ SvgA.fill (colorToCss ts.color)
            , SvgA.fontWeight (if ts.bold then "bold" else "normal")
            , SvgA.fontStyle (if ts.italic then "italic" else "normal")
            , SvgA.fontSize (ts.height |> Maybe.withDefault 14 |> toString)
            , SvgA.transform
                ("translate(" ++
                  ((E.widthOf te |> toFloat) / -2 |> toString) ++ " 0)")
            ]
            [Svg.text str]

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
            [ [ SvgA.width (toString w)
              , SvgA.height (toString h)
              , SvgA.transform ("translate(" ++ toString (-w/2) ++ " " ++ toString (-h/2) ++ ")") ]
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

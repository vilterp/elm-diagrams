module Diagrams.FullWindow exposing (..)
-- where

{-| Utilities for when you just want to get a diagram on the whole screen.

See `Diagrams.Wiring` docs for more info on `CollageLocation`s.

@docs fullWindowCollageLocFunc, fullWindowCollageLoc, fullWindowUpdates, fullWindowMain, fullWindowView
-}

import Window
import Element as E
import Collage as C

import Html.App as App

--import Diagrams.Wiring exposing (..)
import Diagrams.Svg
import Diagrams.Core exposing (..)
import Diagrams.Type exposing (..)

{-
{-| A location function which always returns a `CollageLocation` in the middle of the window,
filling the whole window. -}
fullWindowCollageLocFunc : CollageLocFunc
fullWindowCollageLocFunc dims =
  { offset = (0.0,0.0), dims = dims }

{-| Signal of full-window collage locations, updating as the window size changes. -}
fullWindowCollageLoc : Signal CollageLocation
fullWindowCollageLoc =
  S.map fullWindowCollageLocFunc floatWindowDims

{-| Signal of location and mouse updates for when diagram is filling the whole screen. -}
fullWindowUpdates : Signal (CollageLocation, PrimMouseEvent)
fullWindowUpdates =
  makeUpdateStream fullWindowCollageLocFunc

{-| The easiest way to get a diagram on the screen:

    main = fullWindowMain (rect 10 10 (justFill <| Solid Color.orange))
-}
fullWindowMain : Diagram t a -> Signal E.Element
fullWindowMain dia =
  S.map (\dims -> fullWindowView dims dia) Window.dimensions

{-|-}
fullWindowView : (Int, Int) -> Diagram t a -> E.Element
fullWindowView (w, h) d =
  C.collage w h [render d]
-}

fullWindowMain : Diagram t a -> Program Never
fullWindowMain dia =
  -- possible to render initially with window size?
  App.program
    { init = ({ width = 800, height = 800 }, Cmd.none)
    , update = \newDims _ -> (newDims, Cmd.none)
    , view = \dims -> Diagrams.Svg.toHtml dims dia
    , subscriptions = \_ ->
        Window.resizes
          (\{width, height} -> { width = toFloat width, height = toFloat height })
    }

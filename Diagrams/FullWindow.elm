module Diagrams.FullWindow exposing (..)
-- where

{-| Utilities for when you just want to get a diagram on the whole screen.

See `Diagrams.Wiring` docs for more info on `CollageLocation`s.

@docs fullWindowProgram
-}

import Window
import Element as E
import Collage as C
import Task

import Html.App

--import Diagrams.Wiring exposing (..)
import Diagrams.Svg
import Diagrams.Core exposing (..)
import Diagrams.Type exposing (..)
import Diagrams.Geom exposing (Dims)

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


{-| The easiest way to get a diagram on the screen:

    main = fullWindowProgram (rect 10 10 (justFill <| Solid Color.orange))
-}

type Msg a
  = DiagramMsg a
  | Resize Dims


fullWindowProgram : { view : m -> Diagram t a, model : m, update : a -> m -> m } -> Program Never
fullWindowProgram app =
  let
    initialDims =
      { width = 800
      , height = 800
      }

    toFloatDims size =
      Resize
        { width = toFloat size.width
        , height = toFloat size.height
        }

    update msg (dims, state) =
      case msg of
        DiagramMsg dMsg ->
          ((dims, app.update dMsg state), Cmd.none)

        Resize newDims ->
          ((newDims, state), Cmd.none)
  in
    Html.App.program
      { init =
          ( (initialDims, app.model)
            -- ugh this is a Task Never; which I didn't have to tag it
          , Task.perform (always (Resize initialDims)) toFloatDims Window.size
          )
      , update = update
      , view = \(dims, state) -> Diagrams.Svg.toHtml dims (app.view state) |> Html.App.map DiagramMsg
      , subscriptions = \_ -> Window.resizes toFloatDims
      }


fullWindowShow : Diagram t a -> Program Never
fullWindowShow dia =
  fullWindowProgram { view = always dia, model = (), update = \_ _ -> () }

module SuperBasic exposing (..)

import Color
import Text as T
import String

import Diagrams.Core as Diagrams exposing (..)
import Diagrams.Envelope as Diagrams exposing (..)
import Diagrams.Debug exposing (..)
import Diagrams.Align exposing (..)
import Diagrams.FillStroke exposing (..)
import Diagrams.FullWindow exposing (..)
import Diagrams.Type exposing (..)
import Diagrams.Layout as Layout
import Diagrams.Pad exposing (..)


type CallTree
  = CallNode
      { name : String
      , args : List String
      , retVal : String
      , subCalls : List CallTree
      }


boxHeight =
  20


spaceBetweenCalls =
  30


view : CallTree -> Diagram t a
view (CallNode node) =
  let
    callSpacer =
      hspace spaceBetweenCalls

    subCalls =
      hcatA BottomA ([callSpacer] ++ (List.intersperse callSpacer (List.map view node.subCalls)) ++ [callSpacer])
      |> alignCenter

    callText =
      node.name ++ "(" ++ (String.join ", " node.args) ++ ")"

    callFlexRow =
      [ Layout.block (text T.defaultStyle callText)
      , Layout.spring
      , Layout.block (text T.defaultStyle node.retVal)
      ]

    thisCall =
      Layout.layout
        [ [Layout.strut (width subCalls)]
        , callFlexRow
        ]
      |> alignCenter
      |> background (justSolidFill Color.orange)
      --rect (width subCalls) boxHeight (justSolidFill Color.orange)
  in
    subCalls `above` thisCall


testTree =
  CallNode
    { name = "fib"
    , args = ["3"]
    , retVal = "6"
    , subCalls =
        [ CallNode
            { name = "fib"
            , args = ["2"]
            , subCalls =
                [ CallNode
                    { name = "fib"
                    , args = ["1"]
                    , retVal = "1"
                    , subCalls = []
                    }
                , CallNode
                    { name = "fib"
                    , args = ["0"]
                    , retVal = "1"
                    , subCalls = []
                    }
                ]
            , retVal = "2"
            } 
        , CallNode
            { name = "fib"
            , args = ["1"]
            , retVal = "1"
            , subCalls = []
            }
        ]
    }


main =
  view testTree
  |> fullWindowShow

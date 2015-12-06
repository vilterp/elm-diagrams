module GraphEditor.View where

import Text as T
import Graphics.Collage as C
import Color
import List as L
import Dict as D
import Maybe as M

import Debug

import Diagrams.Core exposing (..)
import Diagrams.Type exposing (..)
import Diagrams.Align exposing (..)
import Diagrams.Pad exposing (..)
import Diagrams.Geom exposing (..)
import Diagrams.Bezier exposing (..)
import Diagrams.Layout exposing (..)
import Diagrams.FillStroke exposing (..)
import Diagrams.Actions exposing (..)
import Diagrams.Query exposing (..)
import Diagrams.Debug exposing (..)

import GraphEditor.Model exposing (..)

-- Styles

defaultTextStyle = T.defaultStyle
titleStyle = { defaultTextStyle | bold = True }

defaultLineStyle = C.defaultLine

edgeStyle = { defaultLineStyle | width = 3 }

defLine = C.defaultLine

-- actions

-- TODO: this is pretty verbose
posNodeActions nodeId dragState =
    case dragState of
      Nothing -> { emptyActionSet | mouseDown = Just <| stopBubbling <|
                                      \(MouseEvent evt) -> [DragNodeStart { nodeId = nodeId, offset = evt.offset }] }
      _ -> emptyActionSet

-- BUG: click isn't working here
nodeXOutActions nodeId = { emptyActionSet | click = Just <| keepBubbling <| always <| [RemoveNode nodeId] }

edgeXOutActions edge = { emptyActionSet | click = Just <| keepBubbling <| always <| [RemoveEdge edge] }

canvasActions dragState =
    let dragMove = { emptyActionSet | mouseMove = Just <| stopBubbling <| \(MouseEvent evt) -> [DragMove evt.offset]
                                    , mouseUp = Just <| stopBubbling <| always [DragEnd] }
    in case dragState of
         Nothing -> emptyActionSet
         _ -> dragMove

outPortActions : PortId -> ActionSet Tag Action
outPortActions portId = { emptyActionSet | mouseDown = Just <| stopBubbling <|
                                              \evt -> [ DragEdgeStart { fromPort = portId
                                                                      , endPos = M.withDefault (0,0) <| mousePosAtPath evt [Canvas]
                                                                      } ] }

inPortActions : PortId -> Maybe DraggingState -> ActionSet Tag Action
inPortActions portId dragState =
    case dragState of
      Just (DraggingEdge attrs) -> { emptyActionSet | mouseUp = Just <| stopBubbling <|
                                                        always <| [AddEdge { from = attrs.fromPort, to = portId }] }
      _ -> emptyActionSet

-- views

xGlyph = let smallLine = vline 11 { defLine | color = Color.white, width = 2 }
             rotLeft = rotate (-pi/4) smallLine
             rotRight = rotate (pi/4) smallLine
             bg = circle 7 <| justFill <| Solid Color.red
         in zcat [rotLeft, rotRight, bg]

-- TODO: can cache diagram in PosNode to improve performance
viewPosNode : Maybe DraggingState -> PosNode -> Diagram Tag Action
viewPosNode dState pn = move pn.pos <| tagWithActions (NodeIdT pn.id) (posNodeActions pn.id dState) <| viewNode pn.node pn.id dState

viewNode : Node -> NodeId -> Maybe DraggingState -> Diagram Tag Action
viewNode node nodeId dState =
   let -- top row
       title = text titleStyle node.title
       xOut = tagWithActions XOut (nodeXOutActions nodeId) <| xGlyph
       titleRow = flexCenter title xOut
       -- ports
       portCirc = circle 7 (justFill <| Solid Color.yellow)
       label lbl = text T.defaultStyle lbl
       -- in ports
       inSlot lbl = flexRight <| hcat [tagWithActions (InPortT lbl) (inPortActions (nodeId, lbl) dState) <| portCirc, hspace 5, label lbl]
       inSlots = L.map inSlot node.inPorts
       -- out ports
       outSlot lbl = flexLeft <| hcat [label lbl, hspace 5, tagWithActions (OutPortT lbl) (outPortActions (nodeId, lbl)) portCirc]
       outSlots = L.map outSlot node.outPorts
       -- pad
       padded = padSpecific 5 5 7 7 <| layout <| [titleRow] ++ inSlots ++ outSlots
   in background (fillAndStroke (Solid Color.orange) defaultStroke) padded

viewEdge : Diagram Tag Action -> Edge -> Diagram Tag Action
viewEdge nodesDia edg =
   let {from, to} = getEdgeCoords nodesDia edg
   in viewGenericEdge from to

viewGenericEdge : Point -> Point -> Diagram Tag Action
viewGenericEdge fromCoords toCoords =
   let (fcx, fcy) = fromCoords
       (tcx, tcy) = toCoords
       cpSpacing = 100
   in bezier fromCoords (fcx+cpSpacing, fcy)
             (tcx-cpSpacing, tcy) toCoords
             edgeStyle

viewDraggingEdge : PortId -> Diagram Tag Action -> Point -> Diagram Tag Action
viewDraggingEdge (fromNode, fromPort) nodesDia mousePos =
  let
    fromCoords =
      case getCoords nodesDia [NodeIdT fromNode, OutPortT fromPort] of 
        Just pt -> pt
        Nothing -> Debug.crash "coords not found"
  in
    viewGenericEdge fromCoords mousePos

getEdgeCoords : Diagram Tag Action -> Edge -> { from : Point, to : Point }
getEdgeCoords nodesDia edg =
    let (fromNode, fromPort) = edg.from
        (toNode, toPort) = edg.to
        -- TODO: not sure how to not have these incomplete pattern matches
        fromCoords =
          case getCoords nodesDia [NodeIdT fromNode, OutPortT fromPort] of
            Just pt -> pt
            Nothing -> Debug.crash "coords not found"
        toCoords =
          case getCoords nodesDia [NodeIdT toNode, InPortT toPort] of
            Just pt -> pt
            Nothing -> Debug.crash "coords not found"
    in { from = fromCoords, to = toCoords }

viewEdgeXOut : Diagram Tag Action -> Edge -> Diagram Tag Action
viewEdgeXOut nodesDia edge =
  let edgeCoords = getEdgeCoords nodesDia edge
  in tagWithActions XOut (edgeXOutActions edge) <| move edgeCoords.to <| xGlyph

view : State -> Diagram Tag Action
view state = 
    let nodes = zcat <| L.map (viewPosNode state.dragState) <| D.values state.graph.nodes
        edges = zcat <| L.map (viewEdge nodes) state.graph.edges
        edgeXOuts = zcat <| L.map (viewEdgeXOut nodes) state.graph.edges
        draggingEdge = case state.dragState of
                         Just (DraggingEdge attrs) -> [viewDraggingEdge attrs.fromPort nodes attrs.endPos]
                         _ -> []
    in tagWithActions Canvas (canvasActions state.dragState) <| pad 10000 <| zcat <| draggingEdge ++ [edgeXOuts, edges, nodes]
-- TODO: pad 10000 is jank

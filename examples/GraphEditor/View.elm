module GraphEditor.View where

import Text as T
import Graphics.Collage as C
import Color
import List as L
import Dict as D

import Debug

import Diagrams.Core (..)
import Diagrams.Align (..)
import Diagrams.Pad (..)
import Diagrams.Geom (..)
import Diagrams.Bezier (..)
import Diagrams.Layout (..)
import Diagrams.FillStroke (..)
import Diagrams.Actions (..)
import Diagrams.Query (..)
import Diagrams.Debug (..)

import GraphEditor.Model (..)

-- Styles

defaultTextStyle = T.defaultStyle
titleStyle = { defaultTextStyle | bold <- True }

defaultLineStyle = C.defaultLine

edgeStyle = { defaultLineStyle | width <- 3 }

-- Views

defLine = C.defaultLine
xGlyph = let smallLine = vline 11 { defLine | color <- Color.white, width <- 2 }
             rotLeft = rotate (-pi/4) smallLine
             rotRight = rotate (pi/4) smallLine
             bg = circle 7 <| justFill <| C.Solid Color.red
         in zcat [rotLeft, rotRight, bg]

viewPosNode : Maybe DraggingState -> PosNode -> Diagram Tag Action
viewPosNode dState pn = move pn.pos <| tagWithActions (NodeIdT pn.id) (posNodeActions pn.id dState) <| viewNode pn.node pn.id dState

-- TODO: this is fucking terrible
posNodeActions nodeId dragState =
    let isOutPort ppe = case ppe.tag of
                          OutPortT _ -> Just True
                          _ -> Nothing
        maybeStartDrag evtAttrs =
          let overOutPort = not <| L.isEmpty <| L.filterMap isOutPort evtAttrs.pickPath
          in if overOutPort then NoOp
             else DragNodeStart { nodeId = nodeId, offset = evtAttrs.offset }
    in case dragState of
         Nothing -> { emptyActionSet | mouseDown <- Just <| \(MouseEvent evt) -> maybeStartDrag evt }
         _ -> emptyActionSet

xOutActions nodeId dragState =
    case dragState of
      Nothing -> { emptyActionSet | click <- Just <| always <| RemoveNode nodeId }
      _ -> emptyActionSet

canvasActions dragState =
    let dragMove = { emptyActionSet | mouseMove <- Just <| \(MouseEvent evt) -> DragMove evt.offset
                                    , mouseUp <- Just <| always DragEnd }
    in case dragState of
         Nothing -> emptyActionSet
         _ -> dragMove

outPortActions : PortId -> ActionSet Tag Action
outPortActions portId = { emptyActionSet | mouseDown <- Just <| \(MouseEvent evt) -> DragEdgeStart { fromPort = portId, endPos = evt.offset } }

inPortActions : PortId -> Maybe DraggingState -> ActionSet Tag Action
inPortActions portId dragState =
    case dragState of
      Just (DraggingEdge attrs) -> { emptyActionSet | mouseUp <- Just <| always <| AddEdge { from = attrs.fromPort, to = portId } }
      _ -> emptyActionSet

viewNode : Node -> NodeId -> Maybe DraggingState -> Diagram Tag Action
viewNode node nodeId dState =
   let -- top row
       title = text node.title titleStyle
       xOut = tagWithActions XOut (xOutActions nodeId dState) <| xGlyph
       titleRow = flexCenter title xOut
       -- ports
       portCirc = circle 7 (justFill <| C.Solid Color.yellow)
       label lbl = text lbl T.defaultStyle
       -- in ports
       inSlot lbl = flexRight <| hcat [tagWithActions (InPortT lbl) (inPortActions (nodeId, lbl) dState) <| portCirc, hspace 5, label lbl]
       inSlots = L.map inSlot node.inPorts
       -- out ports
       outSlot lbl = flexLeft <| hcat [label lbl, hspace 5, tagWithActions (OutPortT lbl) (outPortActions (nodeId, lbl)) portCirc]
       outSlots = L.map outSlot node.outPorts
       -- pad
       padded = padSpecific 5 5 7 7 <| layout <| [titleRow] ++ inSlots ++ outSlots
   in background (fillAndStroke (C.Solid Color.orange) defaultStroke) padded

viewEdge : Diagram Tag Action -> Edge -> Diagram Tag Action
viewEdge nodesDia edg =
   let (fromNode, fromPort) = edg.from
       (toNode, toPort) = edg.to
       fromCoords = case getCoords nodesDia [NodeIdT fromNode, OutPortT fromPort] of { Just pt -> pt }
       toCoords = case getCoords nodesDia [NodeIdT toNode, InPortT toPort] of { Just pt -> pt }
   in viewGenericEdge fromCoords toCoords

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
   let fromCoords = case getCoords nodesDia [NodeIdT fromNode, OutPortT fromPort] of { Just pt -> pt }
   in viewGenericEdge fromCoords mousePos

view : State -> Diagram Tag Action
view state = let nodes = zcat <| L.map (viewPosNode state.dragState) <| D.values state.graph.nodes
                 edges = zcat <| L.map (viewEdge nodes) state.graph.edges
                 draggingEdge = case state.dragState of
                                  Just (DraggingEdge attrs) -> [viewDraggingEdge attrs.fromPort nodes attrs.endPos]
                                  _ -> []
             in tagWithActions Canvas (canvasActions state.dragState) <| pad 10000 <| zcat <| draggingEdge ++ [edges, nodes]

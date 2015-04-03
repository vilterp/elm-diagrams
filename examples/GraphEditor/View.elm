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
slotLabelStyle = defaultTextStyle

defaultLineStyle = C.defaultLine

edgeStyle = { defaultLineStyle | width <- 3 }

defLine = C.defaultLine

nodeTopDivider = defLine
nodeMiddleDivider = { defLine | dashing <- [5, 5] }

-- actions

-- TODO: this is fucking terrible
posNodeActions nodeId dragState =
    case dragState of
      Nothing -> { emptyActionSet | mouseDown <- Just <| stopBubbling <|
                                      \(MouseEvent evt) -> DragNodeStart { nodeId = nodeId, offset = evt.offset } }
      _ -> emptyActionSet

nodeXOutActions nodeId = { emptyActionSet | click <- Just <| keepBubbling <| always <| RemoveNode nodeId }

edgeXOutActions edge = { emptyActionSet | click <- Just <| stopBubbling <| always <| RemoveEdge edge }

canvasActions dragState =
    let dragMove = { emptyActionSet | mouseMove <- Just <| stopBubbling <| \(MouseEvent evt) -> DragMove evt.offset
                                    , mouseUp <- Just <| stopBubbling <| always DragEnd }
    in case dragState of
         Nothing -> emptyActionSet
         _ -> dragMove

outPortActions : OutPortId -> ActionSet Tag Action
outPortActions portId = { emptyActionSet | mouseDown <- Just <| stopBubbling <|
                                              \evt -> DragEdgeStart { fromPort = portId, endPos = collageMousePos evt } }

inPortActions : InPortId -> Maybe DraggingState -> ActionSet Tag Action
inPortActions portId dragState =
    case dragState of
      Just (DraggingEdge attrs) -> { emptyActionSet | mouseUp <- Just <| stopBubbling <|
                                                          always <| AddEdge { from = attrs.fromPort, to = portId } }
      _ -> emptyActionSet

-- views

-- common elements
xGlyph = let smallLine = vline 11 { defLine | color <- Color.white, width <- 2 }
             rotLeft = rotate (-pi/4) smallLine
             rotRight = rotate (pi/4) smallLine
             bg = circle 7 <| justFill <| C.Solid Color.red
         in zcat [rotLeft, rotRight, bg]

-- TODO: color code based on state
portCirc = circle 7 (justFill <| C.Solid Color.yellow)

inSlotLabel : InSlotId -> String
inSlotLabel sid =
    case sid of
      ApParamSlot name -> name
      IfCondSlot -> "condition"
      IfTrueSlot -> "if true"
      IfFalseSlot -> "if false"

inSlot : NodeId -> Maybe DraggingState -> InSlotId -> LayoutRow Tag Action
inSlot nodeId dState slotId =
    flexRight <| hcat [ tagWithActions (InPortT slotId) (inPortActions (nodeId, slotId) dState) <| portCirc
                      , hspace 5
                      , text (inSlotLabel slotId) slotLabelStyle
                      ]

outSlotLabel : OutSlotId -> String
outSlotLabel sid =
    case sid of
      ApResultSlot name -> name
      IfResultSlot -> "result"
      FuncValueSlot -> "" -- not used

outSlot : NodeId -> Maybe DraggingState -> OutSlotId -> LayoutRow Tag Action
outSlot nodeId dState slotId =
    flexLeft <| hcat [ text (outSlotLabel slotId) slotLabelStyle
                     , hspace 5
                     , tagWithActions (OutPortT slotId) (outPortActions (nodeId, slotId)) <| portCirc
                     ]

nodeTitle : String -> NodeId -> Diagram Tag Action
nodeTitle name nodeId =
    let title = text name titleStyle
        xOut = tagWithActions XOut (nodeXOutActions nodeId) <| xGlyph
    in hcat <| [ xOut
               , hspace 5
               , title
               , hspace 5
               ]

type SlotGroup = InputGroup (List InSlotId)
               | OutputGroup (List OutSlotId)

nodeDiagram : NodeId -> Maybe DraggingState -> LayoutRow Tag Action -> List SlotGroup -> Color.Color -> Diagram Tag Action
nodeDiagram nodeId dState titleRow slotGroups color =
    let viewGroup : SlotGroup -> List (LayoutRow Tag Action)
        viewGroup group =
            case group of
              InputGroup ids -> L.map (inSlot nodeId dState) ids
              OutputGroup ids -> L.map (outSlot nodeId dState) ids
    in background (fillAndStroke (C.Solid color) defaultStroke) <|
          layout <| [titleRow, hrule nodeTopDivider 3] ++ (intercalate [hrule nodeMiddleDivider 3] (L.map viewGroup slotGroups))

-- TODO: can cache diagram in PosNode to improve performance
viewPosNode : Maybe DraggingState -> PosNode -> Diagram Tag Action
viewPosNode dState pn = move pn.pos <| tagWithActions (NodeIdT pn.id) (posNodeActions pn.id dState) <| viewNode pn.node pn.id dState

viewNode : Node -> NodeId -> Maybe DraggingState -> Diagram Tag Action
viewNode node nodeId dState =
    case node of
      ApNode attrs -> viewApNode attrs nodeId dState
      IfNode -> viewIfNode nodeId dState

-- TODO: padding is awkward
viewApNode : ApNodeAttrs -> NodeId -> Maybe DraggingState -> Diagram Tag Action
viewApNode node nodeId dState =
    let funcOutPort = tagWithActions (OutPortT FuncValueSlot) (outPortActions (nodeId, FuncValueSlot)) <| portCirc
        titleRow = flexCenter (nodeTitle node.title nodeId) funcOutPort
        params = InputGroup <| L.map ApParamSlot node.params
        results = OutputGroup <| L.map ApResultSlot node.results
    in nodeDiagram nodeId dState titleRow [params, results] Color.lightBlue -- TODO: lighter

viewIfNode : NodeId -> Maybe DraggingState -> Diagram Tag Action
viewIfNode nodeId dState =
    let titleRow = flexRight (nodeTitle "If" nodeId)
        inSlots = InputGroup [IfCondSlot, IfTrueSlot, IfFalseSlot]
        outSlots = OutputGroup [IfResultSlot]
    in nodeDiagram nodeId dState titleRow [inSlots, outSlots] Color.lightPurple

-- edges

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

viewDraggingEdge : OutPortId -> Diagram Tag Action -> Point -> Diagram Tag Action
viewDraggingEdge (fromNode, fromPort) nodesDia mousePos =
   let fromCoords = case getCoords nodesDia [NodeIdT fromNode, OutPortT fromPort] of { Just pt -> pt }
   in viewGenericEdge fromCoords mousePos

getEdgeCoords : Diagram Tag Action -> Edge -> { from : Point, to : Point }
getEdgeCoords nodesDia edg =
    let (fromNode, fromPort) = edg.from
        (toNode, toPort) = edg.to
        -- TODO: not sure how to not have these incomplete pattern matches
        fromCoords = case getCoords nodesDia [NodeIdT fromNode, OutPortT fromPort] of { Just pt -> pt }
        toCoords = case getCoords nodesDia [NodeIdT toNode, InPortT toPort] of { Just pt -> pt }
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

-- util

intercalate : List a -> List (List a) -> List a
intercalate sep xs = L.concat <| L.intersperse sep xs

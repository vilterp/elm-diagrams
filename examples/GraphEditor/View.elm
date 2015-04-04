module GraphEditor.View where

import Text as T
import Graphics.Collage as C
import Color
import List as L
import Dict as D
import Maybe as M

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

-- Styles (TODO: factor into own module?)

defaultTextStyle = T.defaultStyle
titleStyle = { defaultTextStyle | bold <- True }
slotLabelStyle = defaultTextStyle

defaultLineStyle = C.defaultLine

edgeStyle = { defaultLineStyle | width <- 3 }

defLine = C.defaultLine

nodeTopDivider = defLine
nodeMiddleDivider = { defLine | dashing <- [5, 5] }

portColor = Color.yellow

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

-- TODO: don't forget about ports that are taken
portStateColorCode : PortState -> Color.Color
portStateColorCode st = case st of
                          ValidPort -> Color.lightGreen
                          InvalidPort -> Color.grey
                          NormalPort -> Color.yellow
                          TakenPort -> takenColor

takenColor = Color.lightGreen

-- views

-- common elements
xGlyph : Color.Color -> Maybe Color.Color -> Diagram Tag Action
xGlyph lineColor bgColor =
  let smallLine = vline 11 { defLine | color <- lineColor, width <- 2 }
      rotLeft = rotate (-pi/4) smallLine
      rotRight = rotate (pi/4) smallLine
      actualBgColor = M.withDefault (Color.red `withAlpha` 0) bgColor
      bg = circle 7 <| justFill <| C.Solid actualBgColor
  in zcat [rotLeft, rotRight, bg]

nodeXGlyph = xGlyph Color.white Nothing
edgeXGlyph bgC = xGlyph Color.black <| Just bgC

-- TODO: color code based on state
portCirc color = circle 7 (justFill <| C.Solid color)

inSlotLabel : InSlotId -> String
inSlotLabel sid =
    case sid of
      ApParamSlot name -> name
      IfCondSlot -> "condition"
      IfTrueSlot -> "if true"
      IfFalseSlot -> "if false"

inSlot : State -> InPortId -> LayoutRow Tag Action
inSlot state (nodeId, slotId) =
    let stateColor = portStateColorCode <| inPortState state (nodeId, slotId)
    in flexRight <| hcat [ tagWithActions (InPortT slotId) (inPortActions (nodeId, slotId) state.dragState)
                              <| portCirc stateColor
                         , hspace 5
                         , text (inSlotLabel slotId) slotLabelStyle
                         ]

outSlotLabel : OutSlotId -> String
outSlotLabel sid =
    case sid of
      ApResultSlot name -> name
      IfResultSlot -> "result"
      FuncValueSlot -> "" -- not used

outSlot : State -> OutPortId -> LayoutRow Tag Action
outSlot state (nodeId, slotId) =
    let stateColor = portStateColorCode <| outPortState state (nodeId, slotId)
    in flexLeft <| hcat [ text (outSlotLabel slotId) slotLabelStyle
                        , hspace 5
                        , tagWithActions (OutPortT slotId) (outPortActions (nodeId, slotId))
                            <| portCirc stateColor
                        ]

nodeTitle : String -> NodeId -> Diagram Tag Action
nodeTitle name nodeId =
    let title = text name titleStyle
        xOut = tagWithActions XOut (nodeXOutActions nodeId) <| nodeXGlyph
    in hcat <| [ xOut
               , hspace 5
               , title
               , hspace 5
               ]

type SlotGroup = InputGroup (List InSlotId)
               | OutputGroup (List OutSlotId)

nodeDiagram : NodeId -> State -> LayoutRow Tag Action -> List SlotGroup -> Color.Color -> Diagram Tag Action
nodeDiagram nodeId state titleRow slotGroups color =
    let viewGroup : SlotGroup -> List (LayoutRow Tag Action)
        viewGroup group =
            case group of
              InputGroup ids -> L.map (\inSlotId -> inSlot state (nodeId, inSlotId)) ids
              OutputGroup ids -> L.map (\outSlotId -> outSlot state (nodeId, outSlotId)) ids
    in background (fillAndStroke (C.Solid color) defaultStroke) <|
          layout <| [titleRow, hrule nodeTopDivider 3] ++ (intercalate [hrule nodeMiddleDivider 3] (L.map viewGroup slotGroups))

-- TODO: can cache diagram in PosNode to improve performance
viewPosNode : State -> PosNode -> Diagram Tag Action
viewPosNode state pn =
  viewNode pn.node pn.id state
  |> tagWithActions (NodeIdT pn.id) (posNodeActions pn.id state.dragState)
  |> move pn.pos

viewNode : Node -> NodeId -> State -> Diagram Tag Action
viewNode node nodeId state =
    case node of
      ApNode attrs -> viewApNode attrs nodeId state
      IfNode -> viewIfNode nodeId state

-- TODO: padding is awkward
viewApNode : ApNodeAttrs -> NodeId -> State -> Diagram Tag Action
viewApNode node nodeId state =
    let funcOutPortColor = portStateColorCode <| outPortState state (nodeId, FuncValueSlot)
        funcOutPort = tagWithActions (OutPortT FuncValueSlot) (outPortActions (nodeId, FuncValueSlot))
                          <| portCirc funcOutPortColor
        titleRow = flexCenter (nodeTitle node.title nodeId) funcOutPort
        params = InputGroup <| L.map ApParamSlot node.params
        results = OutputGroup <| L.map ApResultSlot node.results
    in nodeDiagram nodeId state titleRow [params, results] Color.lightBlue -- TODO: lighter

viewIfNode : NodeId -> State -> Diagram Tag Action
viewIfNode nodeId state =
    let titleRow = flexRight (nodeTitle "If" nodeId)
        inSlots = InputGroup [IfCondSlot, IfTrueSlot, IfFalseSlot]
        outSlots = OutputGroup [IfResultSlot]
    in nodeDiagram nodeId state titleRow [inSlots, outSlots] Color.lightPurple

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
  in tagWithActions XOut (edgeXOutActions edge) <| move edgeCoords.to <| edgeXGlyph takenColor

view : State -> Diagram Tag Action
view state = 
    let nodes = zcat <| L.map (viewPosNode state) <| D.values state.graph.nodes
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

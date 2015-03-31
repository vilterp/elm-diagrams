module GraphEditor where

import Diagrams.Core (..)
import Diagrams.Query (..)
import Diagrams.Interact (..)
import Diagrams.Wiring as DW
import Diagrams.Geom (..)
import Diagrams.Debug (..)
import Diagrams.Align (..)
import Diagrams.Pad (..)
import Diagrams.Actions (..)
import Diagrams.FillStroke (..)
import Diagrams.FullWindow as DFW
import Diagrams.Envelope (..)
import Diagrams.Bezier (..)
import Diagrams.Layout (..)

import Graphics.Collage as C
import Color
import Text as T
import List as L
import Signal as S
import Debug
import Mouse
import Window
import Maybe as M
import Dict as D

-- graph components

type alias NodeId = String
type alias SlotId = String
type alias PortId = (NodeId, SlotId)

type alias PosNode = { pos : Point, id : NodeId, node : Node, diagram : Diagram Tag Action }
type alias Node = { title : String, inPorts : List SlotId, outPorts : List SlotId }

type alias Edge = { from : PortId, to : PortId }

-- graph

type alias Model = { nodes : D.Dict NodeId PosNode, edges : List Edge }
initModel = { nodes = D.fromList [ (fooPosNode.id, fooPosNode)
                                 , (barPosNode.id, barPosNode)
                                 , (bazPosNode.id, bazPosNode)
                                 ]
            , edges = [fooBarEdge] }

-- TODO: dragging abstraction in Diagrams.Interact or something?
type alias ModelAndDrag = { model : Model, dragState : Maybe DraggingState }
initModelAndDrag = { model = initModel, dragState = Nothing }

-- app state

-- TODO: mouse pos should be a part of this
type DraggingState = DraggingNode { nodeId : NodeId, offset : Point }
                   | DraggingEdge { fromPort : PortId }
type alias State = { modelAndDrag : ModelAndDrag, diagram : Diagram Tag Action }
initState = { modelAndDrag = initModelAndDrag, diagram = view initModelAndDrag (0,0) }

-- DATA

makePosNode : Node -> Point -> NodeId -> PosNode
makePosNode n p id = { pos = p, id = id, node = n, diagram = viewNode n }

fooNode = { title = "Foo", inPorts = ["InAasdfasdfsdafasdfs", "asdfs", "InB", "InC"], outPorts = ["out1", "out2"] }
fooPosNode = makePosNode fooNode (-300, 100) "foo"

bazNode = { title = "Baz", inPorts = ["InA", "InB", "InC"], outPorts = ["out1", "out2"] }
bazPosNode = makePosNode bazNode (100, -200) "baz"

barNode = { title = "Bar", inPorts = ["InA", "InB", "InC"], outPorts = ["out1", "out2"] }
barPosNode = makePosNode barNode (100, 100) "bar"

fooBarEdge = { from = ("foo", "out1"), to = ("bar", "InA") }
--fooBazEdge = { from = ("foo", "out2"), to = ("baz", "InC") }

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
         in tag CloseNode <| zcat [rotLeft, rotRight, bg]

type Tag = NodeIdT NodeId
         | TitleT
         | InPortT SlotId
         | OutPortT SlotId
         | CloseNode

type Action = Action

viewPosNode : PosNode -> Diagram Tag Action
viewPosNode pn = move pn.pos <| tag (NodeIdT pn.id) <| pn.diagram

viewNode : Node -> Diagram Tag Action
viewNode node =
    let -- top row
        title = tag TitleT <| text node.title titleStyle
        titleRow = flexCenter title xGlyph
        -- ports
        portCirc = circle 7 (justFill <| C.Solid Color.yellow)
        label lbl = text lbl T.defaultStyle
        -- in ports
        inSlot lbl = flexRight <| hcat [tag (InPortT lbl) portCirc, hspace 5, label lbl]
        inSlots = L.map inSlot node.inPorts
        -- out ports
        outSlot lbl = flexLeft <| hcat [label lbl, hspace 5, tag (OutPortT lbl) portCirc]
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

view : ModelAndDrag -> Point -> Diagram Tag Action
view mAndD mousePos = let nodes = zcat <| L.map viewPosNode <| D.values mAndD.model.nodes
                          edges = zcat <| L.map (viewEdge nodes) mAndD.model.edges
                          draggingEdge = case mAndD.dragState of
                                           Just (DraggingEdge {fromPort}) -> [viewDraggingEdge fromPort nodes mousePos]
                                           _ -> []
                      in zcat <| draggingEdge ++ [edges, nodes]

-- Main

getDragState : PickPath Tag a -> Maybe DraggingState
getDragState pp = Nothing
  --case pp of
  --  (TitleT, _)::(NodeIdT nid, offset)::_ -> Just <| DraggingNode { nodeId = nid, offset = offset }
  --  (OutPortT slotId, _)::(NodeIdT nid, _)::_ -> Just <| DraggingEdge { fromPort = (nid, slotId) }
  --  _ -> Nothing

moveNode : Model -> { nodeId : NodeId, offset : Point } -> Point -> Model
moveNode model ds mousePos =
  let updateFn value = case value of
                         Just posNode -> let (mx, my) = mousePos
                                             (ox, oy) = ds.offset
                                             newPos = (mx - ox, my - oy)
                                         in Just { posNode | pos <- newPos }
                         Nothing -> Nothing
  in { model | nodes <- D.update ds.nodeId updateFn model.nodes }

addEdge : Model -> PortId -> PortId -> Model
addEdge model from to = let newEdge = { from = from, to = to }
                        in { model | edges <- newEdge :: model.edges }

updateDragState : State -> Maybe DraggingState -> State
updateDragState state ds =
    let maD = state.modelAndDrag
        newMaD = { maD | dragState <- ds }
    in { state | modelAndDrag <- newMaD }

updateModel : State -> Model -> Point -> State
updateModel state newModel mousePos =
    let maD = state.modelAndDrag
        newMaD = { maD | model <- newModel }
    in { state | modelAndDrag <- newMaD
               , diagram <- view newMaD mousePos }

-- TODO: replace with one from interact
upstate : (DW.CollageLocation, DW.MouseEvent) -> State -> State
upstate (collageLoc, (evt, mousePos)) state =
  case evt of
    DW.MouseDownEvt -> let overPath = pick state.diagram mousePos
                       in updateDragState state <| getDragState overPath
    DW.MouseUpEvt -> case state.modelAndDrag.dragState of
                        Just (DraggingNode _) ->
                            updateDragState state Nothing
                        Just (DraggingEdge {fromPort}) ->
                            let overPath = pick state.diagram mousePos
                                newModel = case L.map .tag overPath of
                                             (InPortT inPort)::(NodeIdT nodeId)::_ ->
                                                  addEdge state.modelAndDrag.model fromPort (nodeId, inPort)
                                             _ -> state.modelAndDrag.model
                            in updateModel (updateDragState state Nothing) newModel mousePos
                        _ -> state
    DW.MouseMoveEvt -> let over = Debug.watch "over" <| pick state.diagram mousePos
                    in case state.modelAndDrag.dragState of
                         Nothing -> state
                         Just (DraggingNode dsn) ->
                             let newModel = moveNode state.modelAndDrag.model dsn mousePos
                             in updateModel state newModel mousePos
                         Just (DraggingEdge edg) ->
                             { state | diagram <- view state.modelAndDrag mousePos }

watchedUpstate evt state = let newState = upstate evt state
                               ds = Debug.watch "dragState" newState.modelAndDrag.dragState
                           in newState

state : Signal State
state = S.foldp watchedUpstate initState DFW.fullWindowUpdates

-- TODO: would be nice to wrap this up a bit more
main = S.map2 (\state wh -> DFW.fullWindowView wh state.diagram) state Window.dimensions

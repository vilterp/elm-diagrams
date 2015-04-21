module GraphEditor.Model where

import Diagrams.Geom exposing (Point)

import Dict as D
import List as L

-- data structures

type alias NodeId = String -- TODO: nested nodes. this should be a path
type alias SlotId = String
type alias PortId = (NodeId, SlotId)

-- TODO: abstract out diagram caching (...)
type alias PosNode = { pos : Point, id : NodeId, node : Node }
-- TODO: more node types
type alias Node = { title : String, inPorts : List SlotId, outPorts : List SlotId }

type alias Edge = { from : PortId, to : PortId }

-- graph

type alias Graph = { nodes : D.Dict NodeId PosNode, edges : List Edge }

-- app state

type DraggingState = DraggingNode { nodeId : NodeId, offset : Point }
                   | DraggingEdge { fromPort : PortId, endPos : Point }

type alias State = { graph : Graph, dragState : Maybe DraggingState }

-- tags

type Tag = NodeIdT NodeId
         | TitleT
         | InPortT SlotId
         | OutPortT SlotId
         | XOut
         | Canvas

type Action = DragNodeStart { nodeId : NodeId, offset : Point }
            | DragEdgeStart { fromPort : PortId, endPos : Point }
            | DragMove Point
            | DragEnd
            | RemoveNode NodeId
            | RemoveEdge Edge
            | AddEdge Edge
            | NoOp

-- operations

moveNode : Graph -> NodeId -> Point -> Graph
moveNode model nodeId newPos =
  let updateFn value = case value of
                         Just posNode -> Just { posNode | pos <- newPos }
                         Nothing -> Nothing
  in { model | nodes <- D.update nodeId updateFn model.nodes }

-- TODO: check dups...
addEdge : Graph -> Edge -> Graph
addEdge model newEdge = { model | edges <- newEdge :: model.edges }

removeNode : Graph -> NodeId -> Graph
removeNode graph nodeId = { graph | nodes <- D.remove nodeId graph.nodes
                                  , edges <- L.filter (\e -> fst e.from /= nodeId && fst e.to /= nodeId) graph.edges }

removeEdge : Graph -> Edge -> Graph
removeEdge graph edge = { graph | edges <- L.filter (\e -> e /= edge) graph.edges }

addNode : PosNode -> Graph -> Graph
addNode node graph = { graph | nodes <- D.insert node.id node graph.nodes }

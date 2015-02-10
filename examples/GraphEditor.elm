module GraphEditor where

import Diagrams (..)
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

type alias PosNode = { pos : Point, id : NodeId, node : Node }
type alias Node = { title : String, inPorts : List SlotId, outPorts : List SlotId }

type alias Edge = { from : PortId, to : PortId }

-- graph

type alias Model = { nodes : D.Dict NodeId PosNode, edges : List Edge }
initModel = { nodes = D.fromList [ (fooPosNode.id, fooPosNode)
                                 , (barPosNode.id, barPosNode)
                                 , (bazPosNode.id, bazPosNode)
                                 ]
            , edges = [fooBarEdge, fooBazEdge] }

-- app state

type alias DraggingState = { nodeId : NodeId, offset : Point }
type alias State = { model : Model, diagram : Diagram Tag, dragState : M.Maybe DraggingState }
initState = { model = initModel, diagram = view initModel, dragState = M.Nothing }

--upstate = ...

mouseEvents = let upDown = S.map (\id -> if id then MouseDownEvt else MouseUpEvt) Mouse.isDown
                  moves = S.map (\pos -> MouseMoveEvt) collageMousePos
                  merged = S.merge upDown moves
              in S.map2 (,) merged collageMousePos

-- wai can't I just watch signals
watch = S.map (\x -> Debug.watch "mouseEvent" x) mouseEvents

type MouseEvent = MouseMoveEvt
                | MouseUpEvt
                | MouseDownEvt

-- DATA

fooNode = { title = "Foo", inPorts = ["InAasdfasdfsdafasdfs", "asdfs", "InB", "InC"], outPorts = ["out1", "out2"] }
fooPosNode = { pos = (-300, -100), id = "foo", node = fooNode }

bazNode = { title = "Baz", inPorts = ["InA", "InB", "InC"], outPorts = ["out1", "out2"] }
bazPosNode = { pos = (100, -200), id = "baz", node = bazNode }

barNode = { title = "Bar", inPorts = ["InA", "InB", "InC"], outPorts = ["out1", "out2"] }
barPosNode = { pos = (100, 100), id = "bar", node = barNode }

fooBarEdge = { from = ("foo", "out1"), to = ("bar", "InA") }
fooBazEdge = { from = ("foo", "out2"), to = ("baz", "InC") }

-- Styles

defaultTextStyle = T.defaultStyle
titleStyle = { defaultTextStyle | bold <- True }

defaultLineStyle = C.defaultLine

edgeStyle = { defaultLineStyle | width <- 3 }

-- Align stuff

type AlignDir = AlignLeft | AlignRight
type AlignFlow a = Align AlignDir (Diagram a)
                 --| Divider (C.LineStyle)

alignFlow : List (AlignFlow a) -> Diagram a
alignFlow aligns = let widths = L.map (\(Align _ dia) -> width dia) aligns
                       maxWidth = L.maximum widths
                       addPadding (Align dir dia) =
                          let widthDiff = maxWidth - (width dia) -- TODO: already calculated (perf)
                              wSpacer = hspace widthDiff
                          in case dir of
                               AlignLeft -> dia `beside` wSpacer
                               AlignRight -> wSpacer `beside` dia
                       padded = L.map addPadding aligns
                   in alignLeft <| padded -- TODO ...

-- Views

type Tag = NodeIdT NodeId
         | TitleT
         | InPortT SlotId
         | OutPortT SlotId

viewPosNode : PosNode -> Diagram Tag
viewPosNode pn = move pn.pos <| tag (NodeIdT pn.id) <| viewNode pn.node

viewNode : Node -> Diagram Tag
viewNode node = let title = Align AlignLeft <| tag TitleT <| text node.title titleStyle
                    portCirc = circle 7 (C.Solid Color.yellow)
                    label lbl = text lbl T.defaultStyle
                    inSlot lbl = Align AlignLeft <| hcat [tag (InPortT lbl) portCirc, hspace 5, label lbl]
                    outSlot lbl = Align AlignRight <| hcat [label lbl, hspace 5, tag (OutPortT lbl) portCirc]
                    outSlots = L.map outSlot node.outPorts
                    inSlots = L.map inSlot node.inPorts
                in background (C.Solid Color.orange) <| padAll 5 5 7 7 <| alignFlow <| [title] ++ inSlots ++ outSlots

viewEdge : Diagram Tag -> Edge -> Diagram Tag
viewEdge nodesDia edg = let (fromNode, fromPort) = edg.from
                            (toNode, toPort) = edg.to
                            fromCoords = case getCoords nodesDia [NodeIdT fromNode, OutPortT fromPort] of { Just pt -> pt }
                            toCoords = case getCoords nodesDia [NodeIdT toNode, InPortT toPort] of { Just pt -> pt }
                            (fcx, fcy) = fromCoords
                            (tcx, tcy) = toCoords
                            cpSpacing = 100
                        in bezier [ fromCoords, (fcx+cpSpacing, fcy)
                                  , (tcx-cpSpacing, tcy), toCoords]
                                  edgeStyle

view : Model -> Diagram Tag
view model = let nodes = zcat <| L.map viewPosNode <| D.values model.nodes
                 edges = zcat <| L.map (viewEdge nodes) model.edges
             in zcat [edges, nodes]

-- Main

overNodeTitle : PickPath Tag -> M.Maybe DraggingState
overNodeTitle pp =
  case pp of
    (TitleT, _)::(NodeIdT nid, offset)::xs -> M.Just { nodeId = nid, offset = offset }
    _ -> M.Nothing

moveNode : Model -> DraggingState -> Point -> Model
moveNode model ds mousePos =
  let updateFn value = case value of
                         Just posNode -> let (mx, my) = mousePos
                                             (ox, oy) = ds.offset
                                             newPos = (mx - ox, my - oy)
                                         in Just { posNode | pos <- newPos }
                         Nothing -> Nothing
  in { model | nodes <- D.update ds.nodeId updateFn model.nodes }

upstate : (MouseEvent, Point) -> State -> State
upstate (evt, mousePos) state =
  case evt of
    MouseDownEvt -> let overPath = case pick state.diagram mousePos of { M.Just pp -> pp; Nothing -> [] }
                        overNode = overNodeTitle overPath
                    in case overNode of
                         M.Nothing -> state
                         M.Just ds -> { state | dragState <- M.Just ds }
    MouseUpEvt -> { state | dragState <- M.Nothing }
    MouseMoveEvt -> let over = Debug.watch "over" <| pick state.diagram mousePos
                    in case state.dragState of
                      M.Nothing -> state
                      M.Just ds -> let newModel = moveNode state.model ds mousePos
                                   in { state | model <- newModel
                                              , diagram <- view newModel }

watchedUpstate evt state = let newState = upstate evt state
                               ds = Debug.watch "dragState" newState.dragState
                           in newState

state : Signal State
state = S.foldp watchedUpstate initState mouseEvents

main = S.map2 (\state wh -> fullWindowView wh state.diagram) state Window.dimensions

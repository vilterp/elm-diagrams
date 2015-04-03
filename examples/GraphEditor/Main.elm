module GraphEditor.Main where

import Dict as D
import List as L
import Signal
import Window

import Diagrams.Core (Diagram)
import Diagrams.Wiring as DW
import Diagrams.FullWindow as DFW
import Diagrams.Interact as DI

import GraphEditor.Model (..)
import GraphEditor.Controller as Cont

-- DATA

fooNode = ApNode { title = "Foo", params = ["InAasdfasdfsdafasdfs", "asdfs", "InB", "InC"], results = ["out1", "out2"] }
fooPosNode = { node = fooNode, pos = (-300, 100), id = "foo" }

bazNode = ApNode { title = "Baz", params = ["InA", "InB", "InC"], results = ["out1", "out2"] }
bazPosNode = { node = bazNode, pos = (100, -200), id = "baz" }

barNode = ApNode { title = "Bar", params = ["InA", "InB", "InC"], results = ["out1", "out2"] }
barPosNode = { node = barNode, pos = (100, 100), id = "bar" }

fooBarEdge = { from = ("foo", ApResultSlot "out1"), to = ("bar", ApParamSlot "InA") }
--fooBazEdge = { from = ("foo", "out2"), to = ("baz", "InC") }

ifNode = IfNode
ifPosNode = { id = "if1", node = ifNode, pos = (-200, 300) }

initGraph = { nodes = D.fromList [ (fooPosNode.id, fooPosNode)
                                 , (barPosNode.id, barPosNode)
                                 , (bazPosNode.id, bazPosNode)
                                 , (ifPosNode.id, ifPosNode)
                                 ]
            , edges = [fooBarEdge] }

initState = { graph = initGraph, dragState = Nothing }

-- start 'er up

diagrams : Signal (Diagram Tag Action)
diagrams = DI.interactFold Cont.update Cont.render DFW.fullWindowCollageLocFunc initState

main = Signal.map2 DFW.fullWindowView Window.dimensions diagrams

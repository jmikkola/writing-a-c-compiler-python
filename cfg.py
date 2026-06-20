# Control Flow Graph

from __future__ import annotations
from dataclasses import dataclass
from typing import Union, List


@dataclass
class Entry:
    pass


@dataclass
class Exit:
    pass


@dataclass
class BlockID:
    id: int


NodeID = Union[Entry, Exit, BlockID]


@dataclass
class BasicBlock:
    node_id: int
    instructions: list
    predecessors: List[NodeID]
    successors: List[NodeID]


@dataclass
class EntryNode:
    successors: List[NodeID]


@dataclass
class ExitNode:
    predecessors: List[NodeID]


Node = Union[BasicBlock, EntryNode, ExitNode]


class Graph:
    def __init__(self, nodes):
        self.nodes = nodes

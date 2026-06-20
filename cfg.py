# Control Flow Graph

from __future__ import annotations
from dataclasses import dataclass
from typing import Union, List


import tacky


@dataclass
class Entry:
    def __hash__(self):
        return hash('Entry')


@dataclass
class Exit:
    def __hash__(self):
        return hash('Exit')


@dataclass
class BlockID:
    id: int

    def __hash__(self):
        return hash(f'BlockID({self.id})')


NodeID = Union[Entry, Exit, BlockID]


@dataclass
class BasicBlock:
    node_id: int
    instructions: list
    predecessors: List[NodeID]
    successors: List[NodeID]

    def pretty_print(self):
        lines = [
            f'BasicBlock {self.node_id}:',
            f'    predecessors: {self.predecessors}',
            f'    successors: {self.successors}',
            f'    instructions:',
        ]
        for instr in self.instructions:
            lines.append('        ' + instr.pretty_print())
        return '\n'.join(lines)


@dataclass
class EntryNode:
    successors: List[NodeID]

    def pretty_print(self):
        lines = [
            f'EntryNode:',
            f'    successors: {self.successors}',
        ]
        return '\n'.join(lines)


@dataclass
class ExitNode:
    predecessors: List[NodeID]

    def pretty_print(self):
        lines = [
            f'ExitNode:',
            f'    predecessors: {self.predecessors}',
        ]
        return '\n'.join(lines)


Node = Union[BasicBlock, EntryNode, ExitNode]


class Graph:
    def __init__(self, blocks):
        entry_node = EntryNode([])
        exit_node = ExitNode([])
        self.nodes = [entry_node, exit_node]
        self.nodes_by_id = {
            Entry(): entry_node,
            Exit(): exit_node,
        }
        self.edges = {}
        self.id_by_label = {}

        for (i, block) in enumerate(blocks):
            node_id = BlockID(i)
            node = BasicBlock(
                node_id=node_id,
                instructions=block,
                predecessors=[],
                successors=[],
            )
            self.nodes.append(node)
            self.nodes_by_id[node_id] = node
            self.max_node_id = node_id
            if isinstance(block[0], tacky.Label):
                self.id_by_label[block[0].name] = node_id

    def add_edge(self, start, end):
        if start not in self.edges:
            self.edges[start] = []
        self.edges[start].append(end)
        self.nodes_by_id[start].successors.append(end)
        self.nodes_by_id[end].predecessors.append(start)

    def get_id_by_label(self, label):
        return self.id_by_label[label]

    def nodes_in_order(self):
        block_nodes = [
            node for node in self.nodes
            if isinstance(node, BasicBlock)
        ]
        return sorted(
            block_nodes,
            key=lambda block: block.node_id.id
        )

    def pretty_print(self):
        lines = ['#### Control Flow Graph ####']
        for node in self.nodes:
            lines.append(node.pretty_print())
        return '\n'.join(lines)

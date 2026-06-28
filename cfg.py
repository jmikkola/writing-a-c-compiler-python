# Control Flow Graph

from __future__ import annotations
from dataclasses import dataclass
from typing import Union, List, Set


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
    block_annotation: set
    instructions: list
    annotations: List[set]
    predecessors: Set[NodeID]
    successors: Set[NodeID]

    def pretty_print(self):
        lines = [
            f'BasicBlock {self.node_id}:',
            f'    predecessors: {self.predecessors}',
            f'    successors: {self.successors}',
            f'    instructions:',
        ]
        for (i, instr) in enumerate(self.instructions):
            lines.append('        ' + instr.pretty_print())
            annotations = self.annotations[i]
            if annotations:
                ats = ', '.join(self.show_annotation(c) for c in annotations)
                lines.append('            copies: ' + ats)

        return '\n'.join(lines)

    def show_annotation(self, copy):
        if isinstance(copy, str):
            return copy
        src = self.show_op(copy.src)
        dst = self.show_op(copy.dst)
        return f'{src}->{dst}'

    def show_op(self, value):
        match value:
            case tacky.Identifier(name):
                return name
            case tacky.Constant(const):
                return str(const)

    def get_node_id(self):
        return self.node_id

    def annotate(self, i, annotation):
        self.annotations[i] = annotation

    def remove_instruction(self, idx):
        self.instructions.pop(idx)
        self.annotations.pop(idx)


@dataclass
class EntryNode:
    successors: Set[NodeID]

    def pretty_print(self):
        lines = [
            f'EntryNode:',
            f'    successors: {self.successors}',
        ]
        return '\n'.join(lines)

    def get_node_id(self):
        return Entry()


@dataclass
class ExitNode:
    predecessors: Set[NodeID]

    def pretty_print(self):
        lines = [
            f'ExitNode:',
            f'    predecessors: {self.predecessors}',
        ]
        return '\n'.join(lines)

    def get_node_id(self):
        return Exit()


Node = Union[BasicBlock, EntryNode, ExitNode]


class Graph:
    def __init__(self, blocks):
        entry_node = EntryNode(set())
        exit_node = ExitNode(set())
        self.nodes = [entry_node, exit_node]
        self.nodes_by_id = {
            Entry(): entry_node,
            Exit(): exit_node,
        }
        self.id_by_label = {}

        for (i, block) in enumerate(blocks):
            node_id = BlockID(i)
            node = BasicBlock(
                node_id=node_id,
                block_annotation=set(),
                instructions=block,
                annotations=[set() for _ in block],
                predecessors=set(),
                successors=set(),
            )
            self.nodes.append(node)
            self.nodes_by_id[node_id] = node
            self.max_node_id = node_id
            if isinstance(block[0], tacky.Label):
                self.id_by_label[block[0].name] = node_id

    def add_edge(self, start, end):
        self.nodes_by_id[start].successors.add(end)
        self.nodes_by_id[end].predecessors.add(start)

    def remove_unreachable_node(self, id):
        ''' remove an unreachable node '''
        # This may leave dangling data in id_by_label, but that shouldn't matter
        node = self.nodes_by_id[id]
        # When multiple nodes are unreachable, they may not be removed in
        # topological order, so node.predecessors may not be empty (it also
        # might not be empty because of loops that cannot be entered)
        for pid in node.predecessors:
            predecessor = self.nodes_by_id[pid]
            predecessor.successors.remove(id)
        for sid in node.successors:
            successor = self.nodes_by_id[sid]
            successor.predecessors.remove(id)
        self._delete_node(id)

    def remove_empty_node(self, id):
        ''' remove a reachable, but empty node '''
        node = self.nodes_by_id[id]
        assert(not node.instructions)
        assert(len(node.successors) == 1)

        # Point the successor to the nodes before this
        successor_id = list(node.successors)[0]
        successor = self.nodes_by_id[successor_id]
        successor.predecessors.remove(id)
        for pred_id in node.predecessors:
            successor.predecessors.add(pred_id)

        # Point predecessors to the next node after this
        for pred_id in node.predecessors:
            predecessor = self.nodes_by_id[pred_id]
            predecessor.successors.remove(id)
            predecessor.successors.add(successor_id)

        self._delete_node(id)

    def _delete_node(self, id):
        del self.nodes_by_id[id]
        self.nodes = [n for n in self.nodes if n.get_node_id() != id]
        if id == self.max_node_id:
            self.max_node_id = self.nodes[-1].node_id

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

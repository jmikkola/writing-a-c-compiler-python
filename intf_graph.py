from __future__ import annotations
from dataclasses import dataclass


@dataclass
class Node:
    operand_id: str
    neighbors: list
    spill_cost: float
    color: int | None
    pruned: bool

    def add_neighbor(self, neighbor):
        self.neighbors.append(neighbor)


@dataclass
class Graph:
    nodes: list

    def add_node(self, node):
        self.nodes.append(node)

    def get_node(self, operand_id):
        for node in self.nodes:
            if node.operand_id == operand_id:
                return node

    def color_graph(self):
        pass

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
        if neighbor in self.neighbors:
            return
        self.neighbors.append(neighbor)


@dataclass
class Graph:
    nodes: list

    def add_node(self, operand_id: str):
        assert(isinstance(operand_id, str))
        node = Node(
            operand_id=operand_id,
            neighbors=[],
            spill_cost=0.0,
            color=None,
            pruned=False,
        )
        self.nodes.append(node)

    def get_node(self, operand_id: str):
        assert(isinstance(operand_id, str))
        for node in self.nodes:
            if node.operand_id == operand_id:
                return node

    def add_edge(self, a: str, b: str):
        self.get_node(a).add_neighbor(b)
        self.get_node(b).add_neighbor(a)

    def __contains__(self, key: str):
        assert(isinstance(key, str))
        return self.get_node(key) is not None

    def color_graph(self):
        pass

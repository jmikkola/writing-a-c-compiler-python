from __future__ import annotations
from dataclasses import dataclass


@dataclass
class Node:
    operand_id: str
    neighbors: list
    spill_cost: float
    color: int | None
    pruned: bool


@dataclass
class Graph:
    nodes: list

    def color_graph(self):
        pass

from __future__ import annotations
from dataclasses import dataclass

import assembly


@dataclass
class Node:
    operand_id: str
    neighbors: list
    spill_cost: float
    color: int | None
    pruned: bool

    def add_neighbor(self, neighbor: assembly.Operand):
        assert(isinstance(neighbor, assembly.Operand))
        if neighbor in self.neighbors:
            return
        self.neighbors.append(neighbor)


@dataclass
class Graph:
    nodes: list
    k: int # The degree to use when coloring

    def add_node(self, operand_id: assembly.Operand):
        assert(isinstance(operand_id, assembly.Operand))
        node = Node(
            operand_id=operand_id,
            neighbors=[],
            spill_cost=0.0,
            color=None,
            pruned=False,
        )
        self.nodes.append(node)

    def get_node(self, operand_id: assembly.Operand):
        assert(isinstance(operand_id, assembly.Operand))
        for node in self.nodes:
            if node.operand_id == operand_id:
                return node

    def add_edge(self, a: assembly.Operand, b: assembly.Operand):
        self.get_node(a).add_neighbor(b)
        self.get_node(b).add_neighbor(a)

    def __contains__(self, key: assembly.Operand):
        assert(isinstance(key, assembly.Operand))
        return self.get_node(key) is not None

    def color_graph(self):
        remaining = self._unpruned_nodes()
        if not remaining:
            return

        # Prune one node
        chosen_node = self._choose_node(remaining)
        chosen_node.pruned = True

        # Color the smaller graph
        self.color_graph()

        # Add this node back in and color it
        colors = self._free_colors(chosen_node)

        if colors:
            if self._is_callee_saved(chosen_node):
                chosen_node.color = max(colors)
            else:
                chosen_node.color = min(colors)
            chosen_node.pruned = False

    def _unpruned_nodes(self):
        return [
            node for node in self.nodes
            if not node.pruned
        ]

    def _choose_node(self, remaining):
        # If there is a node with degree < k, pick it
        for node in remaining:
            degree = self._unpruned_neighbors(node)
            if degree < self.k:
                return node

        chosen_node = remaining[0]
        best_spill_metric = float('inf')
        for node in remaining:
            degree = self._unpruned_neighbors(node)
            spill_metric = node.spill_cost / float(degree)
            if spill_metric < best_spill_metric:
                best_spill_metric = spill_metric
                chosen_node = node

        return chosen_node

    def _unpruned_neighbors(self, node: Node):
        n = 0
        for neighbor_id in node.neighbors:
            if not self.get_node(neighbors_id).pruned:
                n += 1
        return n

    def _free_colors(self, chosen_node: Node):
        colors = set(range(1, self.k + 1))
        for neighbor_id in chosen_node.neighbors:
            neighbor = self.get_node(neighbor_id)
            if neighbor.color is not None:
                colors.remove(neighbor.color)
        return colors

    def _is_callee_saved(self, chosen_node: Node):
        operand_id = chosen_node.operand_id
        return operand_id in ('BX', 'R12', 'R13', 'R14', 'R15')

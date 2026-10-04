from __future__ import annotations
from dataclasses import dataclass

import assembly


@dataclass
class Node:
    operand_id: assembly.Operand
    neighbors: list
    spill_cost: float
    color: int | None
    pruned: bool

    def add_neighbor(self, neighbor: assembly.Operand):
        assert(isinstance(neighbor, assembly.Operand))
        if neighbor in self.neighbors:
            return
        self.neighbors.append(neighbor)

    def remove_neighbor(self, neighbor: assembly.Operand):
        assert(isinstance(neighbor, assembly.Operand))
        self.neighbors = [
            n for n in self.neighbors
            if n != neighbor
        ]

    def pretty_print(self):
        lines = [
            str(self.operand_id),
            f'  spill_cost={self.spill_cost} color={self.color} pruned={self.pruned}',
            f'  neighbors={self.neighbors}',
        ]
        return '\n'.join(lines)


class Graph:
    # k is the degree to use when coloring
    def __init__(self, k: int):
        self.nodes = []
        self.nodes_by_id = dict()
        self.k = k

    def add_node(self, operand_id: assembly.Operand):
        assert(isinstance(operand_id, assembly.Operand))
        if operand_id in self.nodes_by_id:
            return
        node = Node(
            operand_id=operand_id,
            neighbors=[],
            spill_cost=0.0,
            color=None,
            pruned=False,
        )
        self.nodes.append(node)
        self.nodes_by_id[operand_id] = node

    def get_node(self, operand_id: assembly.Operand):
        assert(isinstance(operand_id, assembly.Operand))
        return self.nodes_by_id.get(operand_id)

    def remove_node(self, operand_id: assembly.Operand):
        assert(isinstance(operand_id, assembly.Operand))
        node = self.nodes_by_id[operand_id]
        assert(node.neighbors == [])
        del self.nodes_by_id[operand_id]
        self.nodes.remove(node)

    def add_edge(self, a: assembly.Operand, b: assembly.Operand):
        assert(isinstance(a, assembly.Operand))
        assert(isinstance(b, assembly.Operand))
        self.get_node(a).add_neighbor(b)
        self.get_node(b).add_neighbor(a)

    def remove_edge(self, a: assembly.Operand, b: assembly.Operand):
        assert(isinstance(a, assembly.Operand))
        assert(isinstance(b, assembly.Operand))
        self.get_node(a).remove_neighbor(b)
        self.get_node(b).remove_neighbor(a)

    def are_neighbors(self, a: assembly.Operand, b: assembly.Operand):
        assert(isinstance(a, assembly.Operand))
        assert(isinstance(b, assembly.Operand))
        node = self.get_node(a)
        if not node:
            return False
        return b in node.neighbors

    def __contains__(self, key: assembly.Operand):
        assert(isinstance(key, assembly.Operand))
        return key in self.nodes_by_id

    def pretty_print(self):
        header = f'Graph(k={self.k})'
        lines = [header] + [
            node.pretty_print()
            for node in self.nodes
        ]
        return '\n'.join(lines)

    def merge_nodes(self, to_merge: assembly.Operand, to_keep: assembly.Operand):
        assert(isinstance(to_merge, assembly.Operand))
        assert(isinstance(to_keep, assembly.Operand))

        node_to_remove = self.get_node(to_merge)
        for neighbor in node_to_remove.neighbors:
            self.add_edge(to_keep, neighbor)
            self.remove_edge(to_merge, neighbor)

        self.remove_node(to_merge)

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
            if not self.get_node(neighbor_id).pruned:
                n += 1
        return n

    def _free_colors(self, chosen_node: Node):
        colors = set(range(1, self.k + 1))
        for neighbor_id in chosen_node.neighbors:
            neighbor = self.get_node(neighbor_id)
            if neighbor.color is not None and neighbor.color in colors:
                colors.remove(neighbor.color)
        return colors

    def _is_callee_saved(self, chosen_node: Node):
        operand_id = chosen_node.operand_id
        match operand_id:
            case assembly.Register(r):
                return assembly.is_callee_saved(r)
            case _:
                return False

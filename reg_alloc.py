import intf_graph
import cfg

def allocate_registers(instructions, function_classifications):
    ar = AllocateRegisters(function_classifications)
    return ar.allocate(instructions)


class AllocateRegisters:
    def __init__(self, function_classifications):
        self.function_classifications = function_classifications

    def allocate(self, instructions):
        interference_graph = self.build_graph(instructions)

        self.add_spill_costs(interference_graph, instructions)
        interference_graph.color_graph()

        register_map = self.create_register_map(interference_graph)
        return self.replace_pseudoregs(instructions, register_map)

    def build_graph(self, instructions) -> intf_graph.Graph:
        interference_graph = self.base_graph()
        self.add_pseudoregisters(interference_graph, instructions)
        graph = self.make_control_flow_graph(instructions)
        self.analyze_liveness(graph)
        self.add_edges(graph, interference_graph)
        return interference_graph

    def base_graph(self) -> intf_graph.Graph:
        # Add all registers besides RSP and RBP (which aren't used for expressions) and R10 and R11
        # (which are used in spilling to the stack).
        all_registers = [
            'RAX', 'RBX', 'RCX', 'RDX', 'RDI', 'RSI',
            'R8', 'R9', 'R12', 'R13', 'R14', 'R15',
        ]

        interference_graph = intf_graph.Graph(nodes=[])
        for (i, reg) in enumerate(all_registers):
            # Add the node for the current register
            node = intf_graph.Node(
                operand_id=reg,
                neighbors=[],
                spill_cost=0.0,
                color=None,
                pruned=False,
            )
            interference_graph.add_node(node)

            # Add neighbors
            for j in range(i):
                neighbor = all_registers[j]
                # Add edges in both directions
                node.add_neighbor(neighbor)
                interference_graph.get_node(neighbor).add_neighbor(reg)

        return interference_graph

    def add_pseudoregisters(self, interference_graph: intf_graph.Graph, instructions):
        pass

    def make_control_flow_graph(self, instructions):
        pass

    def analyze_liveness(self, graph: cfg.Graph):
        pass

    def add_edges(self, graph: cfg.Graph, interference_graph: intf_graph.Graph):
        pass

    def add_spill_costs(self, interference_graph: intf_graph.Graph, instructions):
        pass

    def create_register_map(self, interference_graph: intf_graph.Graph):
        pass

    def replace_pseudoregs(self, instructions, register_map):
        pass

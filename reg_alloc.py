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
        pass

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

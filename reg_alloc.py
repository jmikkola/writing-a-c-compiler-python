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

    def build_graph(self, instructions):
        pass

    def add_spill_costs(self, interference_graph, instructions):
        pass

    def create_register_map(self, interference_graph):
        pass

    def replace_pseudoregs(self, instructions, register_map):
        pass

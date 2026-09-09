import assembly
import cfg
import intf_graph


# All registers besides RSP and RBP (which aren't used for expressions) and R10 and R11
# (which are used in spilling to the stack) can be used for expressions.
INT_REGISTERS = [
    'AX', 'BX', 'CX', 'DX', 'DI', 'SI',
    'R8', 'R9', 'R12', 'R13', 'R14', 'R15',
]

# All XMM registers except for XMM14 and XMM15 which are used for spilling
XMM_REGISTERS = [
    'XMM0', 'XMM1', 'XMM2', 'XMM3', 'XMM4', 'XMM5', 'XMM6',
    'XMM7', 'XMM8', 'XMM9', 'XMM10', 'XMM11', 'XMM12', 'XMM13',
]


def allocate_registers(
    instructions,
    function_classifications,
    asm_symbols,
    aliased_variables,
    function_name):

    for is_double in (False, True):
        ar = AllocateRegisters(
            is_double,
            function_classifications,
            asm_symbols,
            aliased_variables,
            function_name
        )
        instructions = ar.allocate(instructions)
    return instructions


class AllocateRegisters:
    def __init__(self, is_double, function_classifications, asm_symbols, aliased_variables, function_name):
        self.is_double = is_double
        self.all_registers = XMM_REGISTERS if is_double else INT_REGISTERS
        self.function_classifications = function_classifications
        self.asm_symbols = asm_symbols
        self.aliased_variables = aliased_variables
        self.function_name = function_name

    def allocate(self, instructions):
        interference_graph = self.build_graph(instructions)
        self.add_spill_costs(interference_graph, instructions)
        interference_graph.color_graph()
        register_map, callee_saved_registers = self.create_register_map(interference_graph)
        return self.replace_pseudoregs(instructions, register_map)

    def build_graph(self, instructions) -> intf_graph.Graph:
        interference_graph = self.base_graph()
        self.add_pseudoregisters(interference_graph, instructions)
        graph = self.make_control_flow_graph(instructions)
        self.analyze_liveness(graph)
        self.add_edges(graph, interference_graph)
        return interference_graph

    def base_graph(self) -> intf_graph.Graph:
        registers = [assembly.Register(r) for r in self.all_registers]

        interference_graph = intf_graph.Graph(nodes=[])
        for (i, reg) in enumerate(registers):
            # Add the node for the current register
            interference_graph.add_node(reg)

            # Add neighbors
            for j in range(i):
                neighbor = registers[j]
                interference_graph.add_edge(reg, neighbor)

        return interference_graph

    def add_pseudoregisters(self, interference_graph: intf_graph.Graph, instructions):
        for instr in instructions:
            for operand in instr.operands():
                self.add_pseudoregister(self, interference_graph, operand)

    def add_pseudoregister(self, interference_graph: intf_graph.Graph, operand: assembly.Operand):
        if not isinstance(operand, assembly.Pseudo):
            return

        name = operand.name
        if name in self.aliased_variables:
            return

        entry = self.asm_symbols[name]
        assert(isinstance(entry, assembly.ObjEntry))
        if entry.is_static:
            return

        # Only add doubles if we are building the graph for doubles, and vice versa
        type_is_double = entry.assembly_type == assembly.Double()
        if type_is_double != self.is_double:
            return

        interference_graph.add_node(operand)

    def make_control_flow_graph(self, instructions):
        blocks = self.partition_blocks(instructions)
        graph = cfg.Graph(blocks)
        self.add_all_edges(graph)
        return graph

    def partition_blocks(self, instructions):
        blocks = []
        current_block = []

        for instruction in instructions:
            match instruction:
                case assembly.Label():
                    # Start a new block at each label
                    if current_block:
                        blocks.append(current_block)
                    current_block = [instruction]

                case assembly.Ret() | assembly.Jmp() | assembly.JmpCC():
                    # End a block at a control flow instruction
                    current_block.append(instruction)
                    blocks.append(current_block)
                    current_block = []

                case _:
                    current_block.append(instruction)

        if current_block:
            blocks.append(current_block)

        return block

    def add_all_edges(self, graph: cfg.Graph):
        graph.add_edge(cfg.Entry(), cfg.BlockID(0))

        for node in graph.nodes:
            if isinstance(node, cfg.EntryNode):
                pass
            if isinstance(node, cfg.ExitNode):
                pass

            node_id = node.node_id
            if node_id == graph.max_node_id:
                next_id = cfg.Exit()
            else:
                next_id = cfg.BlockID(node_id.id + 1)

            last_instr = node.instructions[-1]
            match last_instr:
                case assembly.Ret():
                    graph.add_edge(node_id, cfg.Exit())
                case assembly.Jmp(label):
                    target_id = graph.get_id_by_label(target)
                    graph.add_edge(node_id, target_id)
                case assembly.JmpCC(_, label):
                    target_id = graph.get_id_by_label(label)
                    graph.add_edge(node_id, target_id)
                    graph.add_edge(node_id, next_id)
                case _:
                    graph.add_edge(node_id, next_id)

    def analyze_liveness(self, graph: cfg.Graph):
        l = Liveness(self.function_classifications, self.function_name, self.is_double)
        l.analyze_liveness(graph)

    def add_edges(self, graph: cfg.Graph, interference_graph: intf_graph.Graph):
        ''' add edges to the interference_graph.

        This is to indicate when the registers written by an instruction would
        interfere with the live registers (those read by later instructions),
        so they can use the same hard register
        '''
        for node in graph.nodes:
            if isinstance(node, cfg.EntryNode):
                continue
            if isinstance(node, cfg.ExitNode):
                continue

            for (i, instr) in enumerate(node.instructions):
                updated = instr.updated()
                live_registers = block.annotations[i]

                for l in live_registers:
                    is_mov = type(l) in [assembly.Mov, assembly.Movsx, assembly.MovZeroExtend]
                    if is_mov and l == instr.src:
                        continue

                    for u in updated:
                        if u in interference_graph and l in interference_graph and l != u:
                            interference_graph.add_edge(l, u)

    def add_spill_costs(self, interference_graph: intf_graph.Graph, instructions):
        # Hard registers cannot be spilled, so give them an infinite cost
        for r in self.all_registers:
            register = assembly.Register(r)
            interference_graph.get_node(register).spill_cost = float('inf')

        # Simplest possible algorithm: count usages of each pseudoregister
        for instr in instructions:
            for operand in instr.operands():
                if isinstance(operand, assembly.Pseudo):
                    if operand in interference_graph:
                        interference_graph.get_node(operand).spill_cost += 1

    def create_register_map(self, interference_graph: intf_graph.Graph):
        # Find out what color was given to each hard register
        color_map = {}
        for node in interference_graph.nodes:
            match node.operand_id:
                case assembly.Register(r):
                    color_map[node.color] = r
                case assembly.Pseudo():
                    pass
                case _:
                    raise Exception(
                        f'unexpected interference graph node id: {node.operand_id}'
                    )

        # Find out what hard register a pseudoregister will map to
        register_map = {}
        callee_saved_registers = set()
        for node in interference_graph.nodes:
            match node.operand_id:
                case assembly.Register(r):
                    pass
                case assembly.Pseudo(name):
                    if node.color is not None:
                        hardreg = color_map[node.color]
                        register_map[name] = hardreg
                        if assembly.is_callee_saved(hardreg):
                            callee_saved_registers.add(hardreg)

        return (register_map, callee_saved_registers)

    def replace_pseudoregs(self, instructions, register_map):
        result = []

        for instr in instructions:
            match instr with:
                case assembly.Ret():
                    result.append(instr)
                case assembly.Mov(a_type, src, dst):
                    src = self._map_operand(src, register_map)
                    dst = self._map_operand(dst, register_map)
                    if src != dst:
                        result.append(assembly.Mov(a_type, src, dst))
                case assembly.Movsx(a_type, src, dst):
                    src = self._map_operand(src, register_map)
                    dst = self._map_operand(dst, register_map)
                    result.append(assembly.Movsx(a_type, src, dst))
                case assembly.MovZeroExtend(src_type, dst_type, src, dst):
                    src = self._map_operand(src, register_map)
                    dst = self._map_operand(dst, register_map)
                    result.append(assembly.MovZeroExtend(src_type, dst_type, src, dst))
                case assembly.Lea(src, dst):
                    src = self._map_operand(src, register_map)
                    dst = self._map_operand(dst, register_map)
                    result.append(assembly.Lea(src, dst))
                case assembly.Push(operand):
                    operand = self._map_operand(operand, register_map):
                    result.append(assembly.Push(operand))
                case assembly.Pop(reg):
                    reg = self._map_operand(reg, register_map)
                    result.append(assembly.Pop(reg))
                case assembly.Call():
                    result.append(instr)
                case assembly.Unary(operator, a_type, operand):
                    operand = self._map_operand(operand, register_map):
                    result.append(assembly.Unary(operator, a_type, operand))
                case assembly.Binary(operator, a_type, src, dst):
                    src = self._map_operand(src, register_map)
                    dst = self._map_operand(dst, register_map)
                    result.append(assembly.Binary(operator, a_type, src, dst))
                case assembly.Cmp(a_type, left, right):
                    left = self._map_operand(left, register_map)
                    right = self._map_operand(right, register_map)
                    result.append(assembly.Cmp(a_type, left, right))
                case assembly.Idiv(a_type, operand):
                    operand = self._map_operand(operand, register_map):
                    result.append(assembly.Idiv(a_type, operand))
                case assembly.Div(a_type, operand):
                    operand = self._map_operand(operand, register_map):
                    result.append(assembly.Div(a_type, operand))
                case assembly.Cdq():
                    result.append(instr)
                case assembly.Jmp():
                    result.append(instr)
                case assembly.JmpCC():
                    result.append(instr)
                case assembly.SetCC(cond_code, operand):
                    operand = self._map_operand(operand, register_map):
                    result.append(assembly.SetCC(cond_code, operand))
                case assembly.Label():
                    result.append(instr)
                case assembly.Cvttsd2si(a_type, src, dst):
                    src = self._map_operand(src, register_map)
                    dst = self._map_operand(dst, register_map)
                    result.append(assembly.Cvttsd2si(a_type, src, dst))
                case assembly.Cvtsi2sd(a_type, src, dst):
                    src = self._map_operand(src, register_map)
                    dst = self._map_operand(dst, register_map)
                    result.append(assembly.Cvtsi2sd(a_type, src, dst))
                case _:
                    raise Exception(f'unhandled instruction {instr}')

        return result

    def _map_operand(self, operand, register_map):
        match operand:
            case assembly.Pseudo(name):
                if name in register_map:
                    return assembly.Register(register_map[name]))
                else:
                    return operand
            case _:
                return operand


# Analzye the liveness of the control flow graph
class Liveness:
    def __init__(self, function_classifications, function_name: str, is_double: bool):
        self.function_classifications = function_classifications

        # Figure out which relevant registers the function uses for returning
        (_arg_registers, return_registers) = function_classifications[function_name]
        if is_double:
            filtered_registers = [
                r for r in return_registers
                if r.startswith('XMM')
            ]
        else:
            filtered_registers = [
                r for r in return_registers
                if not r.startswith('XMM')
            ]
        self.return_vars = set([
            assembly.Register(r)
            for r in filtered_registers
        ])

    def analyze_liveness(self, graph: cfg.Graph):
        nodes_in_order = graph.nodes_in_order()
        for block in nodes_in_order:
            block.block_annotation = set()

        worklist = [n.node_id for n in nodes_in_order]
        while worklist:
            block_id = worklist.pop(0)
            block = graph.nodes_by_id[block_id]
            old_annotation = block.block_annotation

            end_live_registers = self.meet(graph, block)
            self.transfer(block, end_live_registers)

            if block.block_annotation != old_annotation:
                for predecessor_id in block.predecessors:
                    match predecessor_id:
                        case cfg.Entry():
                            continue
                        case cfg.BlockID():
                            if predecessor_id not in worklist:
                                worklist.append(predecessor_id)
                        case cfg.Exit():
                            raise Exception('block cannot have Exit as a predecessor')

    def meet(self, graph: cfg.Graph, block: cfg.BasicBlock) -> set:
        live_vars = set()

        for succ_id in block.successors:
            match succ_id:
                case cfg.Exit():
                    # This doesn't try to handle callee-saved registers because they will be dealt
                    # with in the instruction fix-up pass, if we end up using them.
                    live_vars |= self.return_vars
                case cfg.Entry():
                    raise Exception('malformed cfg')
                case cfg.BlockID():
                    successor = graph.nodes_by_id[succ_id]
                    live_vars |= successor.block_annotation

        return live_vars

    def transfer(self, block: cfg.BasicBlock, end_live_registers: set):
        current_live_registers = end_live_registers

        # iterate backwards through the instructions
        for i in range(len(block.instructions)-1, -1, -1):
            current_live_registers = clone(current_live_registers)
            block.annotations[i] = current_live_registers

            instr = block.instructions[i]
            used = instr.used_registers(self.function_classifications)
            updated = instr.updated()

            for v in updated:
                if isinstance(v, assembly.Register):
                    current_live_registers -= set([v])

            for v in used:
                if isinstance(v, assembly.Register):
                    current_live_registers.add(v)

        block.block_annotation = current_live_registers

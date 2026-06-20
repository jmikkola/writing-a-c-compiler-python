import math

import cfg
import syntax
import tacky
import typeconversion


def optimize(ir: tacky.Program, args, symbols):
    optimizer = Optimizer(symbols)
    return optimizer.optimize(ir, args)


class Optimizer:
    def __init__(self, symbols):
        self.symbols = symbols

    def optimize(self, ir: tacky.Program, args) -> tacky.Program:
        optimized = []
        for decl in ir.top_level:
            match decl:
                case tacky.Function():
                    optimized.append(self.optimize_function(decl, args))
                case _:
                    optimized.append(decl)
        return tacky.Program(optimized)

    def optimize_function(self, f: tacky.Function, args):
        body = f.body
        if not body:
            return f

        while True:
            if args.fold_constants:
                post_constant_folding = self.constant_folding(body)
            else:
                post_constant_folding = body

            graph = self.make_control_flow_graph(post_constant_folding)
            # print(graph.pretty_print())

            if args.eliminate_unreachable_code:
                graph = self.unreachable_code_elimination(graph)

            if args.propagate_copies:
                graph = self.copy_propagation(graph)

            if args.eliminate_dead_stores:
                graph = self.dead_store_elimination(graph)

            optimized_function_body = self.cfg_to_instructions(graph)

            if optimized_function_body == body:
                break
            if not optimized_function_body:
                break

            body = optimized_function_body

        return tacky.Function(f.name, f.is_global, f.params, body)

    def constant_folding(self, body):
        optimized = []
        for instr in body:
            match instr:
                case tacky.Unary(op, tacky.Constant(const), dst):
                    dst_type = self.value_type(dst)
                    result = self.evaluate_unary_op(op, const, dst_type)
                    new_const = tacky.Constant(result)
                    optimized.append(tacky.Copy(new_const, dst))

                case tacky.Binary(op, tacky.Constant(left), tacky.Constant(right), dst):
                    dst_type = self.value_type(dst)
                    result = self.evaluate_binary_op(op, left, right, dst_type)
                    new_const = tacky.Constant(result)
                    optimized.append(tacky.Copy(new_const, dst))

                case tacky.JumpIfZero(tacky.Constant(const), target):
                    if is_zero(const):
                        optimized.append(tacky.Jump(target))

                case tacky.JumpIfNotZero(tacky.Constant(const), target):
                    if not is_zero(const):
                        optimized.append(tacky.Jump(target))

                case tacky.Truncate(tacky.Constant(const), dst):
                    dst_type = self.value_type(dst)
                    truncated = self.truncate_constant(const, dst_type)
                    optimized.append(tacky.Copy(truncated, dst))

                case tacky.SignExtend(tacky.Constant(const), dst):
                    dst_type = self.value_type(dst)
                    extended = self.sign_extend(const, dst_type)
                    optimized.append(tacky.Copy(extended, dst))

                case tacky.ZeroExtend(tacky.Constant(const), dst):
                    dst_type = self.value_type(dst)
                    extended = self.zero_extend(const, dst_type)
                    optimized.append(tacky.Copy(extended, dst))

                case tacky.DoubleToInt(tacky.Constant(const), dst):
                    dst_type = self.value_type(dst)
                    new_const = self.double_to_int(const, dst_type)
                    optimized.append(tacky.Copy(new_const, dst))

                case tacky.DoubleToUInt(tacky.Constant(const), dst):
                    dst_type = self.value_type(dst)
                    new_const = self.double_to_uint(const, dst_type)
                    optimized.append(tacky.Copy(new_const, dst))

                case tacky.IntToDouble(tacky.Constant(const), dst):
                    new_const = self.int_to_double(const)
                    optimized.append(tacky.Copy(new_const, dst))

                case tacky.UIntToDouble(tacky.Constant(const), dst):
                    new_const = self.uint_to_double(const)
                    optimized.append(tacky.Copy(new_const, dst))

                case _:
                    optimized.append(instr)
        return optimized

    def value_type(self, value: tacky.Value) -> syntax.Type:
        match value:
            case tacky.Constant(tacky.ConstChar(value)):
                return syntax.Char()
            case tacky.Constant(tacky.ConstInt(value)):
                return syntax.Int()
            case tacky.Constant(tacky.ConstLong(value)):
                return syntax.Long()
            case tacky.Constant(tacky.ConstUInt(value)):
                return syntax.UInt()
            case tacky.Constant(tacky.ConstULong(value)):
                return syntax.ULong()
            case tacky.Constant(tacky.ConstDouble(value)):
                return syntax.Double()
            case tacky.Constant(c):
                raise Exception(f'unhandled type of constant {c}')
            case tacky.Identifier(name):
                return self.symbols[name].type
            case _:
                raise Exception(f'unexpected value {value}')

    def truncate_constant(self, const: tacky.Const, dst_type: syntax.Type) -> tacky.Constant:
        truncated = const.value & make_mask(dst_type)
        result = self.as_type(truncated, dst_type)
        return tacky.Constant(result)

    def sign_extend(self, const: tacky.Const, dst_type: syntax.Type) -> tacky.Constant:
        # no actual change needed in the python representation
        extended = const.value
        result = self.as_type(extended, dst_type)
        return tacky.Constant(result)

    def zero_extend(self, const: tacky.Const, dst_type: syntax.Type) -> tacky.Constant:
        src_type = self.value_type(tacky.Constant(const))
        extended = const.value & make_mask(src_type)
        result = self.as_type(extended, dst_type)
        return tacky.Constant(result)

    def double_to_int(self, const: tacky.Const, dst_type: syntax.Type) -> tacky.Constant:
        d = const.value
        # Doesn't handle NaN or out of bounds values
        value = int(d)
        result = self.as_type(value, dst_type)
        return tacky.Constant(result)

    def double_to_uint(self, const: tacky.Const, dst_type: syntax.Type) -> tacky.Constant:
        d = const.value
        if d < 0:
            d = 0
        # Doesn't handle NaN or out of bounds values
        value = int(d)
        result = self.as_type(value, dst_type)
        return tacky.Constant(result)

    def int_to_double(self, const: tacky.Const) -> tacky.Constant:
        value = float(const.value)
        result = tacky.ConstDouble(value)
        return tacky.Constant(result)

    def uint_to_double(self, const: tacky.Const) -> tacky.Constant:
        value = float(const.value)
        result = tacky.ConstDouble(value)
        return tacky.Constant(result)

    def as_type(self, value, ctype: syntax.Type) -> tacky.Const:
        match ctype:
            case syntax.Char() | syntax.SChar() | syntax.UChar():
                return tacky.ConstChar(value & mask_for_bytes(1))
            case syntax.Int():
                return tacky.ConstInt(value & mask_for_bytes(4))
            case syntax.Long():
                return tacky.ConstLong(value & mask_for_bytes(8))
            case syntax.UInt():
                return tacky.ConstUInt(value & mask_for_bytes(4))
            case syntax.ULong():
                return tacky.ConstULong(value & mask_for_bytes(8))
            case syntax.Double():
                return tacky.ConstDouble(value)
            case syntax.Pointer():
                return tacky.ConstULong(value & mask_for_bytes(8))
            case _:
                raise Exception(f'unhandled type for as_type: {ctype}')

    def evaluate_unary_op(self, op: tacky.UnaryOp, const: tacky.Const, dst_type: syntax.Type):
        match op:
            case tacky.UnaryNegate():
                value = -const.value
            case tacky.UnaryInvert():
                value = const.value ^ make_mask(dst_type)
            case tacky.UnaryNot():
                value = int(not const.value)
            case _:
                raise Exception(f'unhandled unary operator {op}')
        return self.as_type(value, dst_type)

    def evaluate_binary_op(self, op: tacky.BinaryOp, left: tacky.Const, right: tacky.Const, dst_type: syntax.Type):
        match op:
            case tacky.BinaryAdd():
                value = left.value + right.value
            case tacky.BinarySubtract():
                value = left.value - right.value
            case tacky.BinaryMultiply():
                value = left.value * right.value
            case tacky.BinaryDivide():
              if dst_type == syntax.Double():
                  if is_zero(right):
                      value = float('nan')
                  else:
                      value = left.value / right.value
              else:
                  if is_zero(right):
                      value = 0
                  else:
                      value = left.value // right.value
            case tacky.BinaryRemainder():
                if is_zero(right):
                    value = 0
                else:
                    value = left.value % right.value
            case tacky.BitAnd():
                value = left.value & right.value
            case tacky.BitOr():
                value = left.value | right.value
            case tacky.BitXor():
                value = left.value ^ right.value
            case tacky.ShiftLeft():
                value = left.value << right.value
            case tacky.ShiftRight():
                value = left.value >> right.value
            case tacky.Less():
                value = int(left.value < right.value)
            case tacky.LessEqual():
                value = int(left.value <= right.value)
            case tacky.Greater():
                value = int(left.value > right.value)
            case tacky.GreaterEqual():
                value = int(left.value >= right.value)
            case tacky.Equals():
                value = int(left.value == right.value)
            case tacky.NotEquals():
                value = int(left.value != right.value)
            case _:
                raise Exception(f'unhandled binary operator {op}')
        return self.as_type(value, dst_type)

    def make_control_flow_graph(self, body):
        blocks = self._parition_blocks(body)
        graph = cfg.Graph(blocks)
        self._add_all_edges(graph)
        return graph

    def _parition_blocks(self, body):
        blocks = []
        current_block = []

        for instruction in body:
            match instruction:
                case tacky.Label():
                    # Start a new block at each label
                    if current_block:
                        blocks.append(current_block)
                    current_block = [instruction]

                case tacky.Jump() | tacky.JumpIfZero() | tacky.JumpIfNotZero() | tacky.Return():
                    # End a block at a control flow instruction
                    current_block.append(instruction)
                    blocks.append(current_block)
                    current_block = []

                case _:
                    current_block.append(instruction)

        if current_block:
            blocks.append(current_block)

        return blocks

    def _add_all_edges(self, graph):
        graph.add_edge(cfg.Entry(), cfg.BlockID(0))

        for node in graph.nodes:
            if isinstance(node, cfg.EntryNode):
                continue
            if isinstance(node, cfg.ExitNode):
                continue

            node_id = node.node_id
            if node_id == graph.max_node_id:
                next_id = cfg.Exit()
            else:
                next_id = cfg.BlockID(node_id.id + 1)

            last_instr = node.instructions[-1]
            match last_instr:
                case tacky.Return():
                    graph.add_edge(node_id, cfg.Exit())
                case tacky.Jump(target):
                    target_id = graph.get_id_by_label(target)
                    graph.add_edge(node_id, target_id)
                case tacky.JumpIfZero(_, target):
                    target_id = graph.get_id_by_label(target)
                    graph.add_edge(node_id, target_id)
                    graph.add_edge(node_id, next_id)
                case tacky.JumpIfNotZero(_, target):
                    target_id = graph.get_id_by_label(target)
                    graph.add_edge(node_id, target_id)
                    graph.add_edge(node_id, next_id)
                case _:
                    graph.add_edge(node_id, next_id)

    def unreachable_code_elimination(self, graph):
        self._remove_unreachable_blocks(graph)
        self._remove_useless_jumps(graph)
        self._remove_useless_labels(graph)
        self._remove_empty_nodes(graph)
        return graph

    def _remove_unreachable_blocks(self, graph):
        # Remove unreachable nodes by visiting all reachable nodes
        ids_seen = set([cfg.Entry()])
        search = [graph.nodes_by_id[cfg.Entry()]]
        while search:
            node = search.pop()
            if isinstance(node, cfg.ExitNode):
                continue
            for id in node.successors:
                if id in ids_seen:
                    continue
                ids_seen.add(id)
                search.append(graph.nodes_by_id[id])

        all_ids = set([n.get_node_id() for n in graph.nodes])
        to_remove = all_ids - ids_seen
        for id in to_remove:
            if id != cfg.Exit():
                graph.remove_node(id)

    def _remove_useless_jumps(self, graph):
        # Remove jumps that always go to the next block and thus have no effect
        sorted_nodes = graph.nodes_in_order()
        for i in range(len(sorted_nodes) - 1):
            node = sorted_nodes[i]
            instructions = node.instructions
            if not instructions:
                continue
            last = instructions[-1]
            ends_in_jump = (
                isinstance(last, tacky.Jump) or
                isinstance(last, tacky.JumpIfZero) or
                isinstance(last, tacky.JumpIfNotZero)
            )
            if not ends_in_jump:
                continue
            default_successor = sorted_nodes[i + 1]
            keep_jump = any(
                succ_id != default_successor.node_id
                for succ_id in node.successors
            )
            if not keep_jump:
                node.instructions.pop()

    def _remove_useless_labels(self, graph):
        # Remove labels that no jump instruction points to.
        #
        # This depends on _remove_useless_jumps having removed jumps that are
        # from the prior node.
        sorted_nodes = graph.nodes_in_order()
        for (i, node) in enumerate(sorted_nodes):
            instructions = node.instructions
            if not instructions:
                continue
            starts_with_label = isinstance(instructions[0], tacky.Label)
            if not starts_with_label:
                continue
            if i == 0:
                default_predecessor_id = cfg.Entry()
            else:
                default_predecessor_id = sorted_nodes[i - 1].node_id
            label_used = any(
                pred_id != default_predecessor_id
                for pred_id in node.predecessors
            )
            if not label_used:
                node.instructions.pop(0)

    def _remove_empty_nodes(self, graph):
        # Remove nodes that now have no instructions in them
        sorted_nodes = graph.nodes_in_order()
        for node in sorted_nodes:
            if node.instructions:
                continue
            graph.remove_empty_node(node.node_id)

    def copy_propagation(self, graph):
        # TODO
        return graph

    def dead_store_elimination(self, graph):
        # TODO
        return graph

    def cfg_to_instructions(self, graph):
        instructions = []
        for node in graph.nodes_in_order():
            instructions.extend(node.instructions)
        return instructions


def is_zero(const: tacky.Const):
    ''' this should treat 0.0 and -0.0 as zero '''
    return const.value in (0, 0.0, -0.0)


def make_mask(t: syntax.Type):
    size_bytes = typeconversion.type_size(t, None)
    return mask_for_bytes(size_bytes)


def mask_for_bytes(size_bytes):
    n_bits = size_bytes * 8
    return (1 << n_bits) - 1

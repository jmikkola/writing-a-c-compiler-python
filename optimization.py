import math

import cfg
import symbol
import syntax
import tacky
import typeconversion
from typeconversion import is_signed


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

        # print(self.make_control_flow_graph(f.body).pretty_print())

        while True:
            aliased_vars = self.address_taken_analysis(body)

            if args.fold_constants:
                post_constant_folding = self.constant_folding(body)
            else:
                post_constant_folding = body

            graph = self.make_control_flow_graph(post_constant_folding)

            if args.eliminate_unreachable_code:
                graph = self.unreachable_code_elimination(graph)

            if args.propagate_copies:
                graph = self.copy_propagation(graph, aliased_vars)
            # print('\n\n\n')
            # print(graph.pretty_print())

            if args.eliminate_dead_stores:
                graph = self.dead_store_elimination(graph, aliased_vars)

            optimized_function_body = self.cfg_to_instructions(graph)

            if optimized_function_body == body:
                break
            if not optimized_function_body:
                break

            body = optimized_function_body

        return tacky.Function(f.name, f.is_global, f.params, body)

    def address_taken_analysis(self, body):
        alised_vars = set()

        def add_if_static(var):
            if isinstance(var, tacky.Identifier):
                sym = self.symbols[var.name]
                if isinstance(sym.attrs, symbol.StaticAttr):
                    alised_vars.add(var.name)

        for instr in body:
            match instr:
                case tacky.GetAddress(src, dst):
                    assert(isinstance(src, tacky.Identifier))
                    alised_vars.add(src.name)
                    add_if_static(dst)
                case tacky.Return(val):
                    add_if_static(val)
                case tacky.Truncate(src, dst):
                    add_if_static(src)
                    add_if_static(dst)
                case tacky.SignExtend(src, dst):
                    add_if_static(src)
                    add_if_static(dst)
                case tacky.ZeroExtend(src, dst):
                    add_if_static(src)
                    add_if_static(dst)
                case tacky.DoubleToInt(src, dst):
                    add_if_static(src)
                    add_if_static(dst)
                case tacky.DoubleToUInt(src, dst):
                    add_if_static(src)
                    add_if_static(dst)
                case tacky.IntToDouble(src, dst):
                    add_if_static(src)
                    add_if_static(dst)
                case tacky.UIntToDouble(src, dst):
                    add_if_static(src)
                    add_if_static(dst)
                case tacky.Unary(_operator, src, dst):
                    add_if_static(src)
                    add_if_static(dst)
                case tacky.Binary(_operator, left, right, dst):
                    add_if_static(left)
                    add_if_static(right)
                    add_if_static(dst)
                case tacky.Copy(src, dst):
                    add_if_static(src)
                    add_if_static(dst)
                case tacky.Load(src, dst):
                    add_if_static(src)
                    add_if_static(dst)
                case tacky.Store(src, dst):
                    add_if_static(src)
                    add_if_static(dst)
                case tacky.AddPtr(ptr, _index, _scale, dst):
                    add_if_static(ptr)
                    add_if_static(dst)
                case tacky.CopyToOffset(src, dst, _offset):
                    add_if_static(src)
                    add_if_static(dst)
                case tacky.CopyFromOffset(src, _offset, dst):
                    add_if_static(src)
                    add_if_static(dst)
                case tacky.Jump(_target):
                    pass
                case tacky.JumpIfZero(condition, _target):
                    add_if_static(condition)
                case tacky.JumpIfNotZero(condition, _target):
                    add_if_static(condition)
                case tacky.Label(_name):
                    pass
                case tacky.Call(_func_name, arg_vals, dst):
                    for a in arg_vals:
                        add_if_static(a)
                    if dst is not None:
                        add_if_static(dst)
                case _:
                    raise Exception(f'unhandled instruction {instr}')

        return alised_vars

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
            case syntax.Char() | syntax.SChar():
                value = self.to_signed(value, 1)
                return tacky.ConstChar(value)
            case syntax.UChar():
                value = self.to_unsigned(value, 1)
                return tacky.ConstChar(value)
            case syntax.Int():
                value = self.to_signed(value, 4)
                return tacky.ConstInt(value)
            case syntax.Long():
                value = self.to_signed(value, 8)
                return tacky.ConstLong(value)
            case syntax.UInt():
                value = self.to_unsigned(value, 4)
                return tacky.ConstUInt(value)
            case syntax.ULong():
                value = self.to_unsigned(value, 8)
                return tacky.ConstULong(value)
            case syntax.Double():
                # No conversion needed
                return tacky.ConstDouble(value)
            case syntax.Pointer():
                value = self.to_unsigned(value, 8)
                return tacky.ConstULong(value)
            case _:
                raise Exception(f'unhandled type for as_type: {ctype}')

    def to_signed(self, value, n_bytes):
        width = 8 * n_bytes
        value &= (1 << width) - 1
        if value >= (1 << (width - 1)):
            value -= (1 << width)
        return value

    def to_unsigned(self, value, n_bytes):
        width = 8 * n_bytes
        return value & (1 << width) - 1

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
                      # Fix python rounding shenanigans
                      if (left.value < 0) != (right.value < 0):
                          left = -1 * left.value
                          value = -1 * (left // right.value)
                      else:
                          value = left.value // right.value
            case tacky.BinaryRemainder():
                if is_zero(right):
                    value = 0
                else:
                    value = left.value % right.value
                    # Python's % is mod, C's % is remainder
                    if (left.value < 0) != (right.value < 0):
                        value -= abs(right.value)
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
                graph.remove_unreachable_node(id)

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

    def copy_propagation(self, graph, aliased_vars):
        CopyPropagation(self.symbols, aliased_vars).optimize(graph)
        return graph

    def dead_store_elimination(self, graph, aliased_vars):
        # TODO
        return graph

    def cfg_to_instructions(self, graph):
        instructions = []
        for node in graph.nodes_in_order():
            instructions.extend(node.instructions)
        return instructions


class CopyPropagation:
    def __init__(self, symbols, aliased_vars):
        self.symbols = symbols
        self.aliased_vars = aliased_vars

    def optimize(self, graph: cfg.Graph):
        ''' this modifies the graph in place '''
        self.find_reaching_copies(graph)
        for block in graph.nodes_in_order():
            self.rewrite_block(block)

    def find_reaching_copies(self, graph: cfg.Graph):
        all_copies = self.find_all_copies(graph)

        # Set up provisional annotations
        nodes_in_order = graph.nodes_in_order()
        for block in nodes_in_order:
            block.block_annotation = all_copies

        worklist = [n.node_id for n in nodes_in_order]
        while worklist:
            block_id = worklist.pop(0)
            block = graph.nodes_by_id[block_id]
            old_annotation = block.block_annotation
            incoming_copies = self.meet(graph, block, all_copies)
            self.transfer(block, incoming_copies)
            if block.block_annotation != old_annotation:
                for successor_id in block.successors:
                    match successor_id:
                        case cfg.Entry():
                            raise Exception('blocks cannot have Entry as a successor')
                        case cfg.BlockID():
                            if successor_id not in worklist:
                                worklist.append(successor_id)
                        case cfg.Exit():
                            continue

    def rewrite_block(self, block):
        new_instructions = []
        new_annotations = []
        for (instruction, annotation) in zip(block.instructions, block.annotations):
            rewritten = self.rewrite_instruction(instruction, annotation)
            if rewritten is not None:
                new_instructions.append(rewritten)
                new_annotations.append(annotation)
        block.instructions = new_instructions
        # Update annotations just to fix any gaps left by deleting a Copy instruction
        block.annotation = new_annotations

    def rewrite_instruction(self, instr, reaching_copies):
        match instr:
            case tacky.Copy(src, dst):
                for copy in reaching_copies:
                    if instr == copy or (copy.src == dst and copy.dst == src):
                        return None
                new_src = self.replace_operand(src, reaching_copies)
                return tacky.Copy(new_src, dst)
            case tacky.Unary(operator, src, dst):
                new_src = self.replace_operand(src, reaching_copies)
                return tacky.Unary(operator, new_src, dst)
            case tacky.Binary(operator, left, right, dst):
                new_left = self.replace_operand(left, reaching_copies)
                new_right = self.replace_operand(right, reaching_copies)
                return tacky.Binary(operator, new_left, new_right, dst)
            case tacky.Return(None):
                return instr
            case tacky.Return(val):
                new_val = self.replace_operand(val, reaching_copies)
                return tacky.Return(new_val)
            case tacky.SignExtend(src, dst):
                new_src = self.replace_operand(src, reaching_copies)
                return tacky.SignExtend(new_src, dst)
            case tacky.Truncate(src, dst):
                new_src = self.replace_operand(src, reaching_copies)
                return tacky.Truncate(new_src, dst)
            case tacky.ZeroExtend(src, dst):
                new_src = self.replace_operand(src, reaching_copies)
                return tacky.ZeroExtend(new_src, dst)
            case tacky.DoubleToInt(src, dst):
                new_src = self.replace_operand(src, reaching_copies)
                return tacky.DoubleToInt(new_src, dst)
            case tacky.DoubleToUInt(src, dst):
                new_src = self.replace_operand(src, reaching_copies)
                return tacky.DoubleToUInt(new_src, dst)
            case tacky.IntToDouble(src, dst):
                new_src = self.replace_operand(src, reaching_copies)
                return tacky.IntToDouble(new_src, dst)
            case tacky.UIntToDouble(src, dst):
                new_src = self.replace_operand(src, reaching_copies)
                return tacky.UIntToDouble(new_src, dst)
            case tacky.Load(src_ptr, dst):
                new_src_ptr = self.replace_operand(src_ptr, reaching_copies)
                return tacky.Load(new_src_ptr, dst)
            case tacky.Store(src, dst_ptr):
                new_src = self.replace_operand(src, reaching_copies)
                return tacky.Store(new_src, dst_ptr)
            case tacky.AddPtr(ptr, index, scale, dst):
                new_ptr = self.replace_operand(ptr, reaching_copies)
                return tacky.AddPtr(new_ptr, index, scale, dst)
            case tacky.CopyToOffset(src, dst, offset):
                new_src = self.replace_operand(src, reaching_copies)
                return tacky.CopyToOffset(new_src, dst, offset)
            case tacky.CopyFromOffset(src, offset, dst):
                new_src = self.replace_operand(src, reaching_copies)
                return tacky.CopyFromOffset(new_src, offset, dst)
            case tacky.Jump():
                return instr
            case tacky.JumpIfZero(condition, target):
                new_condition = self.replace_operand(condition, reaching_copies)
                return tacky.JumpIfZero(new_condition, target)
            case tacky.JumpIfNotZero(condition, target):
                new_condition = self.replace_operand(condition, reaching_copies)
                return tacky.JumpIfNotZero(new_condition, target)
            case tacky.Label():
                return instr
            case tacky.Call(func_name, arg_vals, dst):
                new_arg_vals = [
                    self.replace_operand(arg, reaching_copies)
                    for arg in arg_vals
                ]
                return tacky.Call(func_name, new_arg_vals, dst)
            case tacky.GetAddress():
                return instr
            case _:
                raise Exception(f'unhandled instruction in rewrite_instruction: {instr}')

    def replace_operand(self, op, reaching_copies):
        match op:
            case tacky.Constant():
                return op
            case tacky.Identifier(_):
                for copy in reaching_copies:
                    if copy.dst == op:
                        return copy.src
        return op

    def transfer(self, block: cfg.BasicBlock, initial_reaching_copies: set):
        current_reaching_copies = initial_reaching_copies

        for (i, instruction) in enumerate(block.instructions):
            block.annotate(i, current_reaching_copies)

            match instruction:
                case tacky.Copy(src, dst):
                    if tacky.Copy(dst, src) in current_reaching_copies:
                        continue

                    current_reaching_copies = self.kill_copies(dst, current_reaching_copies)

                    src_type = self.get_type(src)
                    dst_type = self.get_type(dst)
                    if src_type == dst_type or is_signed(src_type) == is_signed(dst_type):
                        current_reaching_copies.add(instruction)

                case tacky.Call(_, _, dst):
                    current_reaching_copies = set(
                        copy for copy in current_reaching_copies
                        if self.survives_func_call(copy, dst)
                    )

                case tacky.Store(src, dst_ptr):
                    current_reaching_copies = set(
                        copy for copy in current_reaching_copies
                        if not (self.is_aliased(copy.src) or self.is_aliased(copy.dst))
                    )

                case tacky.Unary(_, _, dst):
                    current_reaching_copies = self.kill_copies(dst, current_reaching_copies)

                case tacky.Binary(_, _, _, dst):
                    current_reaching_copies = self.kill_copies(dst, current_reaching_copies)

                case tacky.SignExtend(_, dst):
                    current_reaching_copies = self.kill_copies(dst, current_reaching_copies)

                case tacky.Truncate(_, dst):
                    current_reaching_copies = self.kill_copies(dst, current_reaching_copies)

                case tacky.ZeroExtend(_, dst):
                    current_reaching_copies = self.kill_copies(dst, current_reaching_copies)

                case tacky.DoubleToInt(_, dst):
                    current_reaching_copies = self.kill_copies(dst, current_reaching_copies)

                case tacky.DoubleToUInt(_, dst):
                    current_reaching_copies = self.kill_copies(dst, current_reaching_copies)

                case tacky.IntToDouble(_, dst):
                    current_reaching_copies = self.kill_copies(dst, current_reaching_copies)

                case tacky.UIntToDouble(_, dst):
                    current_reaching_copies = self.kill_copies(dst, current_reaching_copies)

                case tacky.GetAddress(_, dst):
                    current_reaching_copies = self.kill_copies(dst, current_reaching_copies)

                case tacky.Load(_, dst):
                    current_reaching_copies = self.kill_copies(dst, current_reaching_copies)

                case tacky.AddPtr(_, _, _, dst):
                    current_reaching_copies = self.kill_copies(dst, current_reaching_copies)

                case tacky.CopyToOffset(_, dst, _):
                    current_reaching_copies = self.kill_copies(dst, current_reaching_copies)

                case tacky.CopyFromOffset(_, _, dst):
                    current_reaching_copies = self.kill_copies(dst, current_reaching_copies)

                case tacky.Jump() | tacky.JumpIfZero() | tacky.JumpIfNotZero() | tacky.Label():
                    pass

                case tacky.Return():
                    pass

                case _:
                    raise Exception(f'unhandled instruction {instr}')

        block.block_annotation = current_reaching_copies

    def get_type(self, value: tacky.Value) -> syntax.Type:
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

    def meet(self, graph: cfg.Graph, block: cfg.BasicBlock, all_copies: set) -> set:
        ''' all_copies: all copy instructions in the entire function '''
        incoming_copies = all_copies

        for pred_id in block.predecessors:
            match pred_id:
                case cfg.Entry():
                    return set()
                case cfg.BlockID(_):
                    pred_out_copies = graph.nodes_by_id[pred_id].block_annotation
                    incoming_copies = incoming_copies & pred_out_copies
                case cfg.Exit():
                    raise Exception('Exit cannot be a predecessor of a block')

        return incoming_copies

    def find_all_copies(self, graph: cfg.Graph) -> set:
        all_copies = set()
        for block in graph.nodes_in_order():
            for instruction in block.instructions:
                if isinstance(instruction, tacky.Copy):
                    all_copies.add(instruction)
        return all_copies

    def kill_copies(self, dst, current_reaching_copies):
        assert(isinstance(dst, tacky.Value))
        return set(
            copy for copy in current_reaching_copies
            if copy.src != dst and copy.dst != dst
        )

    def survives_func_call(self, copy, dst):
        if self.is_aliased(copy.src) or self.is_aliased(copy.dst):
            return False
        if dst is not None and (copy.src == dst or copy.dst == dst):
            return False
        return True

    def is_aliased(self, value):
        match value:
            case tacky.Constant():
                return False
            case tacky.Identifier(name):
                return name in self.aliased_vars
            case _:
                raise Exception(f'unhandled value type {value}')


def is_zero(const: tacky.Const):
    ''' this should treat 0.0 and -0.0 as zero '''
    return const.value in (0, 0.0, -0.0)


def make_mask(t: syntax.Type):
    size_bytes = typeconversion.type_size(t, None)
    return mask_for_bytes(size_bytes)


def mask_for_bytes(size_bytes):
    n_bits = size_bytes * 8
    return (1 << n_bits) - 1

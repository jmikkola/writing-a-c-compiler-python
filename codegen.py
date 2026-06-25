from __future__ import annotations
from dataclasses import dataclass
from enum import Enum
import functools
import struct
import typing

import labels
import assembly
import tacky
import symbol
import syntax
import typeconversion


rax = assembly.Register('AX')
rdx = assembly.Register('DX')
rsp = assembly.Register('SP')
r10 = assembly.Register('R10')
r11 = assembly.Register('R11')
xmm1 = assembly.Register('XMM1')
xmm14 = assembly.Register('XMM14')
xmm15 = assembly.Register('XMM15')

byte = assembly.Byte()
longword = assembly.Longword()
quadword = assembly.Quadword()
double = assembly.Double()


def gen(tacky: tacky.Program, symbols: dict, types: dict, label_gen: labels.Labels) ->\
        typing.Tuple[assembly.Program, dict]:
    cg = Codegen(tacky, symbols, types, label_gen)
    return cg.generate(), cg.asm_symbols


class Codegen:
    def __init__(self, tacky, symbols, types, label_gen):
        self.tacky = tacky
        self.symbols = symbols
        self.types = types
        self.label_gen = label_gen
        self.asm_symbols = self.convert_symbols()
        self.arg_registers = ['DI', 'SI', 'DX', 'CX', 'R8', 'R9']
        self.double_registers = ['XMM0', 'XMM1', 'XMM2', 'XMM3', 'XMM4', 'XMM5', 'XMM6', 'XMM7']

        self._doubles = {}

    def new_const_label(self, name):
        return self.label_gen.new_label('_const_' + name)

    def generate(self):
        top_level = [
            self.gen_top_level(d)
            for d in self.tacky.top_level
        ]
        for (value, label, alignment) in self._doubles.values():
            init = symbol.DoubleInit(value)
            constant = assembly.StaticConstant(label, alignment, init)
            top_level.append(constant)
        return assembly.Program(top_level)

    def convert_symbols(self):
        return {
            name: self.convert_symbol(name, sym)
            for (name, sym) in self.symbols.items()
        }

    def convert_symbol(self, name: str, sym: symbol.Symbol) -> assembly.AsmSymbol:
        attrs = sym.attrs
        match attrs:
            case symbol.FuncAttr(is_defined, is_global):
                # Functions that are not defined may return structures of
                # incomplete types, so we can't actually compute if they return
                # on the stack or not (default to False since it doesn't matter)
                return_on_stack = is_defined and self.function_returns_in_memory(name)
                return assembly.FunEntry(is_defined, return_on_stack)
            case symbol.StaticAttr(init, is_global):
                a_type = self.sym_type_to_a_type(sym.type)
                return assembly.ObjEntry(a_type, True, False)
            case symbol.LocalAttr():
                a_type = self.sym_type_to_a_type(sym.type)
                return assembly.ObjEntry(a_type, False, False)
            case symbol.ConstantAttr(init):
                a_type = self.sym_type_to_a_type(sym.type)
                return assembly.ObjEntry(a_type, True, True)
            case _:
                raise Exception(f'unhandled kind of symbol attributes: {attrs} for {name}')

    def gen_top_level(self, top_level):
        match top_level:
            case tacky.Function():
                return self.gen_function(top_level)
            case tacky.StaticVariable():
                return self.gen_static_var(top_level)
            case tacky.StaticConstant():
                return self.gen_static_constant(top_level)
            case _:
                raise Exception(f'unhandled kind of top level declaration: {top_level}')

    def gen_static_constant(self, const: tacky.StaticConstant) -> assembly.StaticConstant:
        alignment = typeconversion.alignment_of(const.const_type, self.types)
        return assembly.StaticConstant(const.name, alignment, const.init)

    def gen_static_var(self, var: tacky.StaticVariable) -> assembly.StaticVariable:
        alignment = typeconversion.alignment_of(var.var_type, self.types)
        inits = var.inits
        if var.var_type == syntax.Double() and inits[0] == 0:
            inits = [symbol.DoubleInit(0.0)]
        return assembly.StaticVariable(
            name=var.name,
            is_global=var.is_global,
            alignment=alignment,
            inits=inits,
        )

    def function_returns_in_memory(self, function_name):
        func_type = self.symbols[function_name].type
        return self.return_type_uses_memory(func_type.ret)

    def gen_function(self, function: tacky.Function) -> assembly.Function:
        # Generate the basic assembly
        return_in_memory = self.function_returns_in_memory(function.name)
        instructions = self.save_arguments([tacky.Identifier(p) for p in function.params], return_in_memory)
        instructions += self.gen_instructions(function.body)

        # Replace pseudo registers with stack locations
        instructions, stack_size = self.replace_pseudo_registers(instructions, return_in_memory)
        if stack_size % 16 != 0:
            stack_size += 16 - (stack_size % 16)

        allocate = assembly.Binary(
            assembly.Sub(),
            quadword,
            assembly.Immediate(stack_size),
            assembly.Register('SP')
        )
        instructions = [allocate] + instructions

        # Fix instructions that are now invalid
        instructions = self.fix_invalid_instructions(instructions)

        return assembly.Function(
            name=function.name,
            is_global=function.is_global,
            instructions=instructions
        )

    def save_arguments(self, params, return_in_memory):
        ''' For simplicity, copy all parameters to the current stack frame '''
        int_reg_args, double_reg_args, stack_params = self.classify_parameters(params, return_in_memory)

        instructions = []

        reg_index = 0
        if return_in_memory:
            # Save the pointer to the return value
            instructions.append(assembly.Mov(
                quadword,
                assembly.Register('DI'),
                assembly.Memory('BP', -8),
            ))
            reg_index = 1

        # Handle arguments that are passed in registers
        for ((a_type, param), reg) in zip(int_reg_args, self.arg_registers[reg_index:]):
            if isinstance(a_type, assembly.ByteArray):
                copy_instructions = self.copy_bytes_from_reg(reg, param, a_type.size)
                instructions.extend(copy_instructions)
            else:
                instructions.append(assembly.Mov(a_type, assembly.Register(reg), param))

        for (param, reg) in zip(double_reg_args, self.double_registers):
            instructions.append(assembly.Mov(double, assembly.Register(reg), param))

        # Handle arguments that are passed on the stack
        stack_offset = 16
        for (a_type, param) in stack_params:
            if isinstance(a_type, assembly.ByteArray):
                copy_instructions = self.copy_bytes(a_type, assembly.Memory('BP', stack_offset), param)
                instructions.extend(copy_instructions)
            else:
                instructions.append(assembly.Mov(a_type, assembly.Memory('BP', stack_offset), param))
            stack_offset += 8

        return instructions

    def replace_pseudo_registers(self, instructions, return_in_memory):
        stack_map = StackMap(self.asm_symbols, return_in_memory)

        updated_instructions = []
        for instr in instructions:
            match instr:
                case assembly.Ret() | assembly.Cdq():
                    pass
                case assembly.Jmp() | assembly.JmpCC() | assembly.Label():
                    pass
                case assembly.Mov(assembly_type, src, dst):
                    src = stack_map.convert_pseudo_register(src)
                    dst = stack_map.convert_pseudo_register(dst)
                    instr = assembly.Mov(assembly_type, src, dst)
                case assembly.Movsx(src_type, dst_type, src, dst):
                    src = stack_map.convert_pseudo_register(src)
                    dst = stack_map.convert_pseudo_register(dst)
                    instr = assembly.Movsx(src_type, dst_type, src, dst)
                case assembly.MovZeroExtend(src_type, dst_type, src, dst):
                    src = stack_map.convert_pseudo_register(src)
                    dst = stack_map.convert_pseudo_register(dst)
                    instr = assembly.MovZeroExtend(src_type, dst_type, src, dst)
                case assembly.Lea(src, dst):
                    src = stack_map.convert_pseudo_register(src)
                    dst = stack_map.convert_pseudo_register(dst)
                    instr = assembly.Lea(src, dst)
                case assembly.Unary(unary_operator, assembly_type, operand):
                    operand = stack_map.convert_pseudo_register(operand)
                    instr = assembly.Unary(unary_operator, assembly_type, operand)
                case assembly.Binary(binary_operator, assembly_type, left, right):
                    left = stack_map.convert_pseudo_register(left)
                    right = stack_map.convert_pseudo_register(right)
                    instr = assembly.Binary(binary_operator, assembly_type, left, right)
                case assembly.Idiv(assembly_type, operand):
                    operand = stack_map.convert_pseudo_register(operand)
                    instr = assembly.Idiv(assembly_type, operand)
                case assembly.Div(assembly_type, operand):
                    operand = stack_map.convert_pseudo_register(operand)
                    instr = assembly.Div(assembly_type, operand)
                case assembly.Cmp(assembly_type, left, right):
                    left = stack_map.convert_pseudo_register(left)
                    right = stack_map.convert_pseudo_register(right)
                    instr = assembly.Cmp(assembly_type, left, right)
                case assembly.SetCC(cond_code, operand):
                    operand = stack_map.convert_pseudo_register(operand)
                    instr = assembly.SetCC(cond_code, operand)
                case assembly.Call(_):
                    pass
                case assembly.Push(operand):
                    operand = stack_map.convert_pseudo_register(operand)
                    instr = assembly.Push(operand)
                case assembly.Cvttsd2si(assembly_type, src, dst):
                    src = stack_map.convert_pseudo_register(src)
                    dst = stack_map.convert_pseudo_register(dst)
                    instr = assembly.Cvttsd2si(assembly_type, src, dst)
                case assembly.Cvtsi2sd(assembly_type, src, dst):
                    src = stack_map.convert_pseudo_register(src)
                    dst = stack_map.convert_pseudo_register(dst)
                    instr = assembly.Cvtsi2sd(assembly_type, src, dst)
                case _:
                    raise Exception(f'unhandled instruction type {instr}')
            updated_instructions.append(instr)
        return (updated_instructions, stack_map.size_used)

    def fix_invalid_instructions(self, instructions):
        ''' Fix invalid instructions.

        E.g. mov instructions that use a memory address as both
        the source and destination
        '''
        updated_instructions = []
        for instr in instructions:
            updated_instructions += self.fix_invalid_instruction(instr)
        return updated_instructions

    def fix_invalid_instruction(self, instr):
        match instr:
            case assembly.Ret():
                return [instr]
            case assembly.Mov():
                return self.fix_mov(instr)
            case assembly.Movsx():
                return self.fix_movsx(instr)
            case assembly.MovZeroExtend():
                return self.fix_mov_zero_extend(instr)
            case assembly.Lea():
                return self.fix_lea(instr)
            case assembly.Push():
                return self.fix_push(instr)
            case assembly.Call():
                return [instr]
            case assembly.Unary():
                return [instr]
            case assembly.Binary():
                return self.fix_binary(instr)
            case assembly.Cmp():
                return self.fix_cmp(instr)
            case assembly.Idiv():
                return self.fix_idiv(instr)
            case assembly.Div():
                return self.fix_div(instr)
            case assembly.Cdq():
                return [instr]
            case assembly.Jmp():
                return [instr]
            case assembly.JmpCC():
                return [instr]
            case assembly.SetCC():
                return [instr]
            case assembly.Label():
                return [instr]
            case assembly.Cvttsd2si():
                return self.fix_cvttsd2si(instr)
            case assembly.Cvtsi2sd():
                return self.fix_cvtsi2sd(instr)
            case _:
                return Exception(f'unhandled instruction type {instr}')

    def fix_cvttsd2si(self, instr: assembly.Cvttsd2si) -> list:
        dst = instr.dst
        a_type = instr.assembly_type
        if not is_register(dst):
            return [
                assembly.Cvttsd2si(a_type, instr.src, r11),
                assembly.Mov(a_type, r11, instr.dst)
            ]
        return [instr]

    def fix_cvtsi2sd(self, instr: assembly.Cvtsi2sd) -> list:
        src = instr.src
        dst = instr.dst
        a_type = instr.assembly_type
        prefix = []
        if is_immediate(src):
            prefix.append(assembly.Mov(a_type, src, r10))
            src = r10
        postfix = []
        if not is_register(dst):
            postfix.append(assembly.Mov(double, xmm15, dst))
            dst = xmm15
        instructions = [assembly.Cvtsi2sd(a_type, src, dst)]
        return prefix + instructions + postfix

    def fix_mov(self, instr: assembly.Mov) -> list:
        assembly_type = instr.assembly_type
        src = instr.src
        dst = instr.dst
        scratch = r10
        if assembly_type == double:
            scratch = xmm14
        if is_immediate(src) and assembly_type == byte:
            src = assembly.Immediate(src.value % 256)
        if is_mem(src) and is_mem(dst):
            return [
                assembly.Mov(assembly_type, src, scratch),
                assembly.Mov(assembly_type, scratch, dst),
            ]
        elif is_large_imm(src) and is_mem(dst):
            return [
                assembly.Mov(assembly_type, src, scratch),
                assembly.Mov(assembly_type, scratch, dst),
            ]
        else:
            return [instr]

    def fix_movsx(self, instr: assembly.Movsx) -> list:
        src_type = instr.src_type
        dst_type = instr.dst_type
        src = instr.src
        dst = instr.dst
        # It can't use a memory address as the destination or and immediate as
        # the source
        if is_immediate(src) and is_mem(dst):
            return [
                assembly.Mov(src_type, src, r10),
                assembly.Movsx(src_type, dst_type, r10, r11),
                assembly.Mov(dst_type, r11, dst),
            ]
        elif is_immediate(src):
            return [
                assembly.Mov(src_type, src, r10),
                assembly.Movsx(src_type, dst_type, r10, dst),
            ]
        elif is_mem(dst):
            return [
                assembly.Movsx(src_type, dst_type, src, r11),
                assembly.Mov(dst_type, r11, dst),
            ]
        else:
            return [instr]

    def fix_mov_zero_extend(self, instr: assembly.MovZeroExtend) -> list:
        src_type = instr.src_type
        dst_type = instr.dst_type
        src = instr.src
        dst = instr.dst
        match src_type:
            case assembly.Longword():
                if is_register(dst):
                    return [assembly.Mov(src_type, src, dst)]
                elif is_mem(dst):
                    return [
                        assembly.Mov(src_type, src, r11),
                        assembly.Mov(dst_type, r11, dst),
                    ]
                else:
                    return [instr]
            case assembly.Byte():
                prefix = []
                if is_immediate(src):
                    prefix = [assembly.Mov(src_type, src, r10)]
                    src = r10
                suffix = []
                if is_mem(dst):
                    suffix = [assembly.Mov(dst_type, r11, dst)]
                    dst = r11
                instruction = [assembly.MovZeroExtend(src_type, dst_type, src, dst)]
                return prefix + instruction + suffix
            case _:
                raise Exception(f'unhandled source type for zero extend: {src_type}')

    def fix_lea(self, instr: assembly.Lea) -> list:
        src = instr.src
        dst = instr.dst
        suffix = []
        if not is_register(dst):
            suffix = [assembly.Mov(quadword, r11, dst)]
            dst = r11
        instr = assembly.Lea(src, dst)
        return [instr] + suffix

    def fix_cmp(self, instr: assembly.Cmp) -> list:
        assembly_type = instr.assembly_type
        left = instr.left
        right = instr.right
        instructions = []

        if assembly_type == double and not is_register(right):
            instructions.append(assembly.Mov(assembly_type, right, xmm15))
            right = xmm15
        elif is_mem(left) and is_mem(right):
            instructions.append(assembly.Mov(assembly_type, left, r10))
            left = r10
        elif is_large_imm(left):
            instructions.append(assembly.Mov(assembly_type, left, r10))
            left = r10

        if is_immediate(right):
            instructions.append(assembly.Mov(assembly_type, right, r11))
            right = r11

        instructions.append(assembly.Cmp(assembly_type, left, right))
        return instructions

    def fix_idiv(self, instr: assembly.Idiv) -> list:
        assembly_type = instr.assembly_type
        assert(assembly_type != double)
        operand = instr.operand
        if is_immediate(operand):
            return [
                assembly.Mov(assembly_type, operand, r10),
                assembly.Idiv(assembly_type, r10),
            ]
        else:
            return [instr]

    def fix_div(self, instr: assembly.Div) -> list:
        assembly_type = instr.assembly_type
        assert(assembly_type != double)
        operand = instr.operand
        if is_immediate(operand):
            return [
                assembly.Mov(assembly_type, operand, r10),
                assembly.Div(assembly_type, r10),
            ]
        else:
            return [instr]

    def fix_binary(self, instr: assembly.Binary) -> list:
        op = instr.binary_operator
        assembly_type = instr.assembly_type
        src = instr.src
        dst = instr.dst

        prefix = []
        suffix = []

        if assembly_type == double and is_mem(dst):
            prefix.append(assembly.Mov(assembly_type, dst, xmm15))
            suffix.append(assembly.Mov(assembly_type, xmm15, dst))
            dst = xmm15

        if op == assembly.Mult() and is_mem(dst):
            # It's important that Mult is handled differently from other
            # binary operations because it can't have a memory address
            # in the destination
            prefix.append(assembly.Mov(assembly_type, dst, r11))
            suffix.append(assembly.Mov(assembly_type, r11, dst))
            dst = r11

        if is_mem(src) and is_mem(dst):
            prefix.append(assembly.Mov(assembly_type, src, r10))
            src = r10

        if is_large_imm(src):
            # The Add, Sub, Mult, etc instructions can't handle immediate values
            # that don't fit in an int
            prefix.append(assembly.Mov(assembly_type, src, r10))
            src = r10

        instr = assembly.Binary(op, assembly_type, src, dst)
        return prefix + [instr] + suffix

    def fix_push(self, instr: assembly.Push) -> list:
        operand = instr.operand
        if is_large_imm(operand):
            return [
                assembly.Mov(quadword, operand, r10),
                assembly.Push(r10),
            ]
        elif isinstance(operand, assembly.Register) and operand.reg.startswith('XMM'):
            return [
                assembly.Binary(assembly.Sub(), quadword, assembly.Immediate(8), rsp),
                assembly.Mov(double, operand, assembly.Memory('SP', 0)),
            ]
        else:
            return [instr]

    def gen_instructions(self, body: list) -> list:
        result = []
        for instruction in body:
            result += self.gen_instruction(instruction)
        return result

    def gen_instruction(self, instr: tacky.Instruction) -> list:
        ''' returns a list of assembly instructions '''
        assert(isinstance(instr, tacky.Instruction))
        match instr:
            case tacky.Return():
                return self.gen_return(instr)
            case tacky.Unary():
                return self.gen_unary(instr)
            case tacky.Binary():
                return self.gen_binary(instr)
            case tacky.Copy(src, dst):
                a_type = self.a_type_of(src)
                if isinstance(a_type, assembly.ByteArray):
                    return self.copy_byte_array(a_type, src, dst)
                return [
                    assembly.Mov(a_type, self.convert_operand(src), self.convert_operand(dst)),
                ]
            case tacky.CopyToOffset(src, dst, offset):
                a_type = self.a_type_of(src)
                if isinstance(a_type, assembly.ByteArray):
                    return self.copy_byte_array(a_type, src, dst, dst_offset=offset)
                asm_src = self.convert_operand(src)
                match dst:
                    case tacky.Identifier(name):
                        asm_dst = assembly.PseudoMem(name, offset)
                    case _:
                        raise Exception(f'invalid operand for CopyToOffset: {dst}')
                return [
                    assembly.Mov(a_type, asm_src, asm_dst),
                ]
            case tacky.CopyFromOffset(src, offset, dst):
                a_type = self.a_type_of(dst)
                if isinstance(a_type, assembly.ByteArray):
                    return self.copy_byte_array(a_type, src, dst, src_offset=offset)
                match src:
                    case tacky.Identifier(name):
                        asm_src = assembly.PseudoMem(name, offset)
                    case _:
                        raise Exception(f'invalid operand for CopyFromOffset: {src}')
                asm_dst = self.convert_operand(dst)
                return [
                    assembly.Mov(a_type, asm_src, asm_dst),
                ]
            case tacky.GetAddress(src, dst):
                return [
                    assembly.Lea(self.convert_operand(src), self.convert_operand(dst)),
                ]
            case tacky.Load(src_ptr, dst):
                a_type = self.a_type_of(dst)
                if isinstance(a_type, assembly.ByteArray):
                    return self.load_byte_array(a_type, src_ptr, dst)
                return [
                    assembly.Mov(quadword, self.convert_operand(src_ptr), rax),
                    assembly.Mov(a_type, assembly.Memory('AX', 0), self.convert_operand(dst)),
                ]
            case tacky.Store(src, dst_ptr):
                a_type = self.a_type_of(src)
                if isinstance(a_type, assembly.ByteArray):
                    return self.store_byte_array(a_type, src, dst_ptr)
                return [
                    assembly.Mov(quadword, self.convert_operand(dst_ptr), rax),
                    assembly.Mov(a_type, self.convert_operand(src), assembly.Memory('AX', 0)),
                ]
            case tacky.AddPtr(ptr, tacky.Constant(const), scale, dst):
                asm_ptr = self.convert_operand(ptr)
                asm_dst = self.convert_operand(dst)
                index = const.value
                assert(isinstance(index, int))
                offset = index * scale
                return [
                    assembly.Mov(quadword, asm_ptr, rax),
                    assembly.Lea(assembly.Memory('AX', offset), asm_dst),
                ]
            case tacky.AddPtr(ptr, index, scale, dst):
                asm_ptr = self.convert_operand(ptr)
                asm_idx = self.convert_operand(index)
                asm_dst = self.convert_operand(dst)
                if scale in [1, 2, 4, 8]:
                    return [
                        assembly.Mov(quadword, asm_ptr, rax),
                        assembly.Mov(quadword, asm_idx, rdx),
                        assembly.Lea(assembly.Indexed('AX', 'DX', scale), asm_dst),
                    ]
                else:
                    asm_scale = assembly.Immediate(scale)
                    return [
                        assembly.Mov(quadword, asm_ptr, rax),
                        assembly.Mov(quadword, asm_idx, rdx),
                        assembly.Binary(assembly.Mult(), quadword, asm_scale, rdx),
                        assembly.Lea(assembly.Indexed('AX', 'DX', 1), asm_dst),
                    ]
            case tacky.Jump(target):
                return [assembly.Jmp(target)]
            case tacky.JumpIfZero(cond, target):
                a_type = self.a_type_of(cond)
                if a_type == double:
                    return [
                        assembly.Binary(assembly.BitXor(), double, xmm1, xmm1),
                        assembly.Cmp(double, self.convert_operand(cond), xmm1),
                        # Check for equal to zero
                        assembly.Mov(longword, assembly.Immediate(0), rax),
                        assembly.SetCC('E', rax),
                        # Check for NaN
                        assembly.Mov(longword, assembly.Immediate(0), rdx),
                        assembly.SetCC('NP', rdx),
                        # Jump based on the combination of the two
                        assembly.Binary(assembly.BitAnd(), longword, rdx, rax),
                        assembly.JmpCC('NE', target),
                    ]
                return [
                    assembly.Cmp(a_type, assembly.Immediate(0), self.convert_operand(cond)),
                    assembly.JmpCC('E', target),
                ]
            case tacky.JumpIfNotZero(cond, target):
                a_type = self.a_type_of(cond)
                if a_type == double:
                    return [
                        assembly.Binary(assembly.BitXor(), double, xmm1, xmm1),
                        assembly.Cmp(double, self.convert_operand(cond), xmm1),
                        assembly.JmpCC('NE', target),
                        assembly.JmpCC('P', target),
                    ]
                return [
                    assembly.Cmp(a_type, assembly.Immediate(0), self.convert_operand(cond)),
                    assembly.JmpCC('NE', target),
                ]
            case tacky.Label(name):
                return [assembly.Label(name)]
            case tacky.Call():
                return self.gen_call(instr)
            case tacky.SignExtend():
                return self.gen_sign_extend(instr)
            case tacky.Truncate():
                return self.gen_truncate(instr)
            case tacky.ZeroExtend():
                return self.gen_zero_extend(instr)
            case tacky.DoubleToInt():
                return self.gen_double_to_int(instr)
            case tacky.DoubleToUInt():
                return self.gen_double_to_uint(instr)
            case tacky.IntToDouble():
                return self.gen_int_to_double(instr)
            case tacky.UIntToDouble():
                return self.gen_uint_to_double(instr)
            case _:
                raise Exception(f'unhandled instruction type, {instr}')

    def copy_bytes_to_reg(self, src_op, dst_reg, byte_count):
        ''' copy one byte at a time from memory to a register '''
        instructions = []
        offset = byte_count - 1
        while offset >= 0:
            # Go in reverse order because the data is little-endian
            src_byte = src_op.add_offset(offset)
            instructions.append(assembly.Mov(
                byte,
                src_byte,
                assembly.Register(dst_reg),
            ))
            if offset > 0:
                instructions.append(assembly.Binary(
                    assembly.ShiftLeft(),
                    quadword,
                    assembly.Immediate(8),
                    assembly.Register(dst_reg),
                ))
            offset -= 1

        return instructions

    def copy_bytes_from_reg(self, src_reg, dst_op, byte_count):
        ''' copy one byte at a time from a register to memory '''
        instructions = []
        offset = 0
        while offset < byte_count:
            dst_byte = dst_op.add_offset(offset)
            instructions.append(assembly.Mov(
                byte,
                assembly.Register(src_reg),
                dst_byte,
            ))
            if offset < byte_count - 1:
                instructions.append(assembly.Binary(
                    assembly.ShiftRight(),
                    quadword,
                    assembly.Immediate(8),
                    assembly.Register(src_reg),
                ))
            offset += 1
        return instructions

    def copy_bytes(self, a_type, src, dst):
        '''unlike the other copy functions, src and dst have already been
        converted to the assembly types'''
        mov_instruction = lambda size, bytes_copied: assembly.Mov(
            size,
            src.add_offset(bytes_copied),
            dst.add_offset(bytes_copied),
        )
        return self._make_copy_movs(a_type, [], mov_instruction)

    def copy_byte_array(self, a_type, src, dst, src_offset=0, dst_offset=0):
        src = self.convert_operand(src)
        dst = self.convert_operand(dst)

        mov_instruction = lambda size, bytes_copied: assembly.Mov(
            size,
            src.add_offset(bytes_copied + src_offset),
            dst.add_offset(bytes_copied + dst_offset),
        )
        return self._make_copy_movs(a_type, [], mov_instruction)

    def load_byte_array(self, a_type, src_ptr, dst):
        dst = self.convert_operand(dst)

        instructions = [
            assembly.Mov(quadword, self.convert_operand(src_ptr), rax),
        ]

        mov_instruction = lambda size, bytes_copied: assembly.Mov(
            size,
            assembly.Memory('AX', bytes_copied),
            dst.add_offset(bytes_copied),
        )

        return self._make_copy_movs(a_type, instructions, mov_instruction)

    def store_byte_array(self, a_type, src, dst_ptr):
        src = self.convert_operand(src)

        instructions = [
            assembly.Mov(quadword, self.convert_operand(dst_ptr), rax),
        ]

        mov_instruction = lambda size, bytes_copied: assembly.Mov(
            size,
            src.add_offset(bytes_copied),
            assembly.Memory('AX', bytes_copied),
        )

        return self._make_copy_movs(a_type, instructions, mov_instruction)

    def _make_copy_movs(self, a_type, instructions, mov_instruction):
        if not isinstance(a_type, assembly.ByteArray):
            raise Exception(f'unhandled type of data for copy movs: {a_type}')
        bytes_copied = 0
        remaining_bytes = a_type.size
        while remaining_bytes >= 8:
            instructions.append(mov_instruction(quadword, bytes_copied))
            remaining_bytes -= 8
            bytes_copied += 8
        while remaining_bytes >= 4:
            instructions.append(mov_instruction(longword, bytes_copied))
            remaining_bytes -= 4
            bytes_copied += 4
        while remaining_bytes > 0:
            instructions.append(mov_instruction(byte, bytes_copied))
            remaining_bytes -= 1
            bytes_copied += 1

        return instructions

    def gen_double_to_int(self, instr: tacky.DoubleToInt) -> list:
        src = self.convert_operand(instr.src)
        dst = self.convert_operand(instr.dst)
        a_type = self.a_type_of(instr.dst)
        if a_type == byte:
            return [
                assembly.Cvttsd2si(longword, src, rax),
                assembly.Mov(byte, rax, dst),
            ]
        return [assembly.Cvttsd2si(a_type, src, dst)]

    def gen_double_to_uint(self, instr: tacky.DoubleToUInt) -> list:
        src = self.convert_operand(instr.src)
        dst = self.convert_operand(instr.dst)
        a_type = self.a_type_of(instr.dst)
        if a_type == byte:
            return [
                assembly.Cvttsd2si(longword, src, rax),
                # Truncate
                assembly.Mov(byte, rax, dst),
            ]
        if a_type == longword:
            return [
                assembly.Cvttsd2si(quadword, src, rdx),
                # Truncate
                assembly.Mov(longword, rdx, dst),
            ]

        assert(a_type == quadword)
        upper_bound_label = self.add_double(9223372036854775808.0)
        upper_bound = assembly.Data(upper_bound_label, 0)
        upper_bound_long = assembly.Immediate(9223372036854775808)
        out_of_range_label = self.label_gen.new_label('out_of_range')
        end_label = self.label_gen.new_label('end')
        return [
            assembly.Cmp(double, upper_bound, src),
            assembly.JmpCC('AE', out_of_range_label),
            assembly.Cvttsd2si(a_type, src, dst),
            assembly.Jmp(end_label),
            assembly.Label(out_of_range_label),
            assembly.Mov(double, src, xmm1),
            assembly.Binary(assembly.Sub(), double, upper_bound, xmm1),
            assembly.Cvttsd2si(a_type, xmm1, dst),
            assembly.Mov(quadword, upper_bound_long, rdx),
            assembly.Binary(assembly.Add(), quadword, rdx, dst),
            assembly.Label(end_label),
        ]

    def gen_int_to_double(self, instr: tacky.IntToDouble) -> list:
        src = self.convert_operand(instr.src)
        dst = self.convert_operand(instr.dst)
        a_type = self.a_type_of(instr.src)
        if a_type == byte:
            return [
                assembly.Movsx(byte, longword, src, rax),
                assembly.Cvtsi2sd(longword, rax, dst),
            ]
        return [assembly.Cvtsi2sd(a_type, src, dst)]

    def gen_uint_to_double(self, instr: tacky.UIntToDouble) -> list:
        src = self.convert_operand(instr.src)
        dst = self.convert_operand(instr.dst)
        a_type = self.a_type_of(instr.src)
        if a_type == byte:
            return [
                assembly.MovZeroExtend(byte, longword, src, rax),
                assembly.Cvtsi2sd(longword, rax, dst),
            ]
        if a_type == longword:
            return [
                assembly.MovZeroExtend(longword, quadword, src, rdx),
                assembly.Cvtsi2sd(quadword, rdx, dst),
            ]

        assert(a_type == quadword)
        out_of_range_label = self.label_gen.new_label('out_of_range')
        end_label = self.label_gen.new_label('end')
        return [
            assembly.Cmp(a_type, assembly.Immediate(0), src),
            assembly.JmpCC('L', out_of_range_label),
            assembly.Cvtsi2sd(a_type, src, xmm1),
            assembly.Jmp(end_label),
            assembly.Label(out_of_range_label),
            assembly.Mov(a_type, src, rax),
            assembly.Mov(a_type, rax, rdx),
            # Divide the source by two
            assembly.Binary(assembly.ShiftRightLogical(), a_type, assembly.Immediate(1), rdx),
            # Grab the low bit of the source
            assembly.Binary(assembly.BitAnd(), a_type, assembly.Immediate(1), rax),
            # Or in the low bit of the source so that rouding in cvtsi2sdq goes in the right direction
            assembly.Binary(assembly.BitOr(), a_type, rax, rdx),
            # Convert to a double
            assembly.Cvtsi2sd(a_type, rdx, xmm1),
            # Double the result
            assembly.Binary(assembly.Add(), double, xmm1, xmm1),
            assembly.Label(end_label),
            assembly.Mov(double, xmm1, dst),
        ]

    def gen_zero_extend(self, instr: tacky.ZeroExtend) -> list:
        src_type = self.a_type_of(instr.src)
        dst_type = self.a_type_of(instr.dst)
        src = self.convert_operand(instr.src)
        dst = self.convert_operand(instr.dst)
        return [assembly.MovZeroExtend(src_type, dst_type, src, dst)]

    def gen_sign_extend(self, instr: tacky.SignExtend) -> list:
        src_type = self.a_type_of(instr.src)
        dst_type = self.a_type_of(instr.dst)
        src = self.convert_operand(instr.src)
        dst = self.convert_operand(instr.dst)
        return [assembly.Movsx(src_type, dst_type, src, dst)]

    def gen_truncate(self, instr: tacky.Truncate) -> list:
        dst_type = self.a_type_of(instr.dst)
        src = self.convert_operand(instr.src)
        if isinstance(src, assembly.Immediate):
            n_bits = dst_type.bytes() * 8
            mask = 2**n_bits - 1
            truncated = src.value & mask
            src = assembly.Immediate(truncated)
        dst = self.convert_operand(instr.dst)
        return [assembly.Mov(dst_type, src, dst)]

    def classify_parameters(self, values: list, return_in_memory):
        '''Decide whether each value passed to a function goes in an integer
        register, an XMM register, or the stack'''
        int_reg_args = []
        double_reg_args = []
        stack_args = []

        if return_in_memory:
            # When the return value goes in memory instead of a register (due to
            # being a larger struct), the first integer register is used to hold
            # the pointer to the memory where the return value will go, so
            # there's one fewer for integer arguments.
            int_regs_available = 5
        else:
            int_regs_available = 6

        for v in values:
            operand = self.convert_operand(v)
            t = self.value_type(v)
            a_type = self.sym_type_to_a_type(t)
            typed_operand = (a_type, operand)
            if t == syntax.Double():
                if len(double_reg_args) < 8:
                    double_reg_args.append(operand)
                else:
                    stack_args.append(typed_operand)
            elif is_scalar(t):
                if len(int_reg_args) < int_regs_available:
                    int_reg_args.append(typed_operand)
                else:
                    stack_args.append(typed_operand)
            else:
                # A struct value
                classes = self.classify_type(t)
                use_stack = True
                struct_size = self.types[t.tag].size
                if classes[0] != MemClass.MEMORY:
                    tentative_ints = []
                    tentative_doubles = []
                    offset = 0
                    for c in classes:
                        operand = assembly.PseudoMem(v.name, offset)
                        if c == MemClass.SSE:
                            tentative_doubles.append(operand)
                        else:
                            eightbyte_type = self.get_eightbyte_type(offset, struct_size)
                            tentative_ints.append((eightbyte_type, operand))
                        offset += 8

                    space_for_doubles = len(tentative_doubles) + len(double_reg_args) <= 8
                    space_for_ints = len(tentative_ints) + len(int_reg_args) <= int_regs_available
                    if space_for_ints and space_for_doubles:
                        double_reg_args.extend(tentative_doubles)
                        int_reg_args.extend(tentative_ints)
                        use_stack = False

                if use_stack:
                    offset = 0
                    for c in classes:
                        operand = assembly.PseudoMem(v.name, offset)
                        eightbyte_type = self.get_eightbyte_type(offset, struct_size)
                        stack_args.append((eightbyte_type, operand))
                        offset += 8

        return (int_reg_args, double_reg_args, stack_args)

    def classify_return_value(self, retval):
        ''' returns ([int args], [double args], in_memory) '''
        t = self.value_type(retval)
        a_type = self.sym_type_to_a_type(t)

        if t == syntax.Double():
            operand = self.convert_operand(retval)
            return ([], [operand], False)

        elif is_scalar(t):
            typed_operand = (a_type, self.convert_operand(retval))
            return ([typed_operand], [], False)

        else:
            # the return value is a struct or a union
            classes = self.classify_type(t)
            struct_size = self.types[t.tag].size
            if classes[0] == MemClass.MEMORY:
                # Return the entire structure in memory
                return ([], [], True)
            else:
                # Partition the returned structure by class
                int_retvals = []
                double_retvals = []
                offset = 0
                for c in classes:
                    operand = assembly.PseudoMem(retval.name, offset)
                    match c:
                        case MemClass.SSE:
                            double_retvals.append(operand)
                        case MemClass.INTEGER:
                            eightbyte_type = self.get_eightbyte_type(offset, struct_size)
                            int_retvals.append((eightbyte_type, operand))
                        case _:
                            raise Exception(f"bug, didn't expect to see mem class {c}")
                    offset += 8

                return (int_retvals, double_retvals, False)

    def return_type_uses_memory(self, t: syntax.Type):
        ''' returns true if the value is returned in memory '''
        if is_scalar(t):
            return False
        if t == syntax.Void():
            return False

        assert(isinstance(t, syntax.Struct) or isinstance(t, syntax.Union))
        classes = self.classify_type(t)
        return classes[0] == MemClass.MEMORY

    def get_eightbyte_type(self, offset: int, struct_size: int) -> assembly.AssemblyType:
        bytes_from_end = struct_size - offset
        if bytes_from_end >= 8:
            return quadword
        if bytes_from_end == 4:
            return longword
        if bytes_from_end == 1:
            return byte
        else:
            return assembly.ByteArray(bytes_from_end, 8)

    @functools.cache
    def classify_type(self, type: syntax.Type) -> typing.List[MemClass]:
        # type must be either a syntax.Union or a syntax.Struct
        # type_entry is either a StructType or a UnionType
        type_entry = self.types[type.tag]

        # Large structs are passed only in memory
        if type_entry.size > 16:
            result = []
            size = type_entry.size
            while size > 0:
                result.append(MemClass.MEMORY)
                size -= 8
            return result

        # Record what types are found in each eightbyte
        mem_use = [MemUse()]
        if type_entry.size > 8:
            mem_use.append(MemUse())

        self.classify_fields(mem_use, type, 0)
        return [m.to_mem_type() for m in mem_use]

    def classify_fields(self, mem_use, type, offset):
        # This scheme of mapping the starting offset of each field to the
        # eightbyte relies on the fact that no scalar type should be able to
        # straddle an eightbyte.
        idx = offset // 8

        match type:
            case syntax.Array(elem_t, size):
                elem_size = typeconversion.type_size(elem_t, self.types)
                for i in range(size):
                    self.classify_fields(mem_use, elem_t, offset + i * elem_size)
            case syntax.Struct(tag):
                struct_type = self.types[tag]
                for member in struct_type.members:
                    self.classify_fields(mem_use, member.type, offset + member.offset)
            case syntax.Union(tag):
                union_type = self.types[tag]
                for member in union_type.members:
                    self.classify_fields(mem_use, member.type, offset)
            case syntax.Double():
                mem_use[idx].double_seen = True
            case _:
                mem_use[idx].int_seen = True

    def gen_call(self, instr: tacky.Call) -> list:
        instructions = []

        return_in_memory = False
        int_dests = []
        double_dests = []
        reg_index = 0

        # Classify the return value
        if instr.dst is not None:
            int_dests, double_dests, return_in_memory = self.classify_return_value(instr.dst)

            if return_in_memory:
                # Set up the pointer so the called function knows where to
                # return the value
                dst_operand = self.convert_operand(instr.dst)
                instructions.append(assembly.Lea(dst_operand, assembly.Register('DI')))
                reg_index = 1

        int_reg_args, double_reg_args, stack_params = self.classify_parameters(instr.arg_vals, return_in_memory)

        # Add stack padding so the alignment comes out right
        stack_padding = 8 * (len(stack_params) % 2)
        if stack_padding > 0:
            allocate = assembly.Binary(
                assembly.Sub(),
                quadword,
                assembly.Immediate(stack_padding),
                assembly.Register('SP')
            )
            instructions.append(allocate)

        # Pass the first 6 (or 5) integer arguments in registers
        # (it's only 5 if the first register is used to hold the pointer to
        # where the return value will be stored)
        assert(len(int_reg_args) <= (len(self.arg_registers) - reg_index))
        for (a_type, arg) in int_reg_args:
            register = self.arg_registers[reg_index]
            match a_type:
                case assembly.ByteArray(size, _alignment):
                    # Structs of weird sizes (e.g. 3 bytes) need special handling
                    copy_instructions = self.copy_bytes_to_reg(arg, register, size)
                    instructions.extend(copy_instructions)
                case _:
                    mov = assembly.Mov(a_type, arg, assembly.Register(register))
                    instructions.append(mov)
            reg_index += 1

        # Pass the first 8 double arguments in XMM registers
        for (arg, register) in zip(double_reg_args, self.double_registers):
            instructions.append(assembly.Mov(double, arg, assembly.Register(register)))

        # Pass the remaining arguments on the stack
        for (a_type, arg) in stack_params[::-1]:
            is_register = isinstance(arg, assembly.Register)
            is_immediate = isinstance(arg, assembly.Immediate)
            if isinstance(a_type, assembly.ByteArray):
                instructions.append(assembly.Binary(
                    assembly.Sub(),
                    quadword,
                    assembly.Immediate(8),
                    assembly.Register('SP'),
                ))
                copy_instructions = self.copy_bytes(a_type, arg, assembly.Memory('SP', 0))
                instructions.extend(copy_instructions)
            elif is_register or is_immediate or a_type == quadword or a_type == double:
                instructions.append(assembly.Push(arg))
            else:
                # Only longword from memory need to be extended to 64 bits before being pushed
                instructions.append(assembly.Mov(a_type, arg, assembly.Register('AX')))
                instructions.append(assembly.Push(assembly.Register('AX')))

        # The actual function call
        instructions.append(assembly.Call(instr.func_name))

        # Clean up the stack
        bytes_to_remove = 8 * len(stack_params) + stack_padding
        if bytes_to_remove > 0:
            deallocate = assembly.Binary(
                assembly.Add(),
                quadword,
                assembly.Immediate(bytes_to_remove),
                assembly.Register('SP')
            )
            instructions.append(deallocate)

        # Move the result to the correct destination
        if instr.dst is not None and not return_in_memory:
            int_return_registers = ['AX', 'DX']
            double_return_registers = ['XMM0', 'XMM1']

            # Retrieve values returned in general-purpose registers
            reg_index = 0
            for (t, op) in int_dests:
                register = int_return_registers[reg_index]
                if isinstance(t, assembly.ByteArray):
                    copy_instructions = self.copy_bytes_from_reg(register, op, t.size)
                    instructions.extend(copy_instructions)
                else:
                    instructions.append(assembly.Mov(t, assembly.Register(register), op))
                reg_index += 1

            # Retrieve values returend in XMM registers
            reg_index = 0
            for op in double_dests:
                register = double_return_registers[reg_index]
                instructions.append(assembly.Mov(double, assembly.Register(register), op))
                reg_index += 1

        return instructions

    def gen_return(self, instr: tacky.Return) -> list:
        retval = instr.val
        if retval is None:
            return [assembly.Ret()]

        int_retvals, double_retvals, return_in_memory = self.classify_return_value(retval)

        instructions = []
        if return_in_memory:
            # Grab the pointer we saved before and put it in RAX
            instructions.append(assembly.Mov(quadword, assembly.Memory('BP', -8), rax))
            # Copy the bytes of the struct to the pointed location
            return_storage = assembly.Memory('AX', 0)
            ret_operand = self.convert_operand(retval)
            a_type = self.a_type_of(retval)
            instructions.extend(
                self.copy_bytes(a_type, ret_operand, return_storage)
            )
        else:
            int_return_registers = ['AX', 'DX']
            double_return_registers = ['XMM0', 'XMM1']

            # Copy the (up to) two integer class values in registers
            reg_index = 0
            for (t, op) in int_retvals:
                register = int_return_registers[reg_index]
                if isinstance(t, assembly.ByteArray):
                    instructions.extend(
                        self.copy_bytes_to_reg(op, register, t.size)
                    )
                else:
                    instructions.append(assembly.Mov(t, op, assembly.Register(register)))
                reg_index += 1

            # Copy the (up to) two sse class values in xmm registers
            reg_index = 0
            for op in double_retvals:
                register = double_return_registers[reg_index]
                instructions.append(assembly.Mov(double, op, assembly.Register(register)))
                reg_index += 1

        instructions.append(assembly.Ret())
        return instructions

    def gen_unary(self, instr: tacky.Unary) -> list:
        src = self.convert_operand(instr.src)
        dst = self.convert_operand(instr.dst)

        a_type = self.a_type_of(instr.src)

        if a_type == double and instr.unary_operator == tacky.UnaryNegate():
            # Alignment has to be 16 because the instruction that loads it is
            # going to load extra data
            label = self.add_double(-0.0, alignment=16)
            # define new constant for -0.0, which has to be aligned to 16 bytes
            # mov the source to the dest
            # xor the constant with the dest
            return [
                assembly.Mov(a_type, src, dst),
                assembly.Binary(assembly.BitXor(), a_type, assembly.Data(label, 0), dst),
            ]

        if a_type == double and instr.unary_operator == tacky.UnaryNot():
            return [
                assembly.Binary(assembly.BitXor(), double, xmm1, xmm1),
                assembly.Cmp(double, src, xmm1),
                assembly.Mov(longword, assembly.Immediate(0), dst),
                assembly.Mov(longword, assembly.Immediate(0), rdx),
                assembly.SetCC('E', dst),
                assembly.SetCC('NP', rdx),
                assembly.Binary(assembly.BitAnd(), self.a_type_of(instr.dst), rdx, dst),
            ]

        if isinstance(instr.unary_operator, tacky.UnaryNot):
            return [
                assembly.Cmp(a_type, assembly.Immediate(0), src),
                # Zero the destination because the 'set' instruction only
                # updates the lowest 8 bits.
                assembly.Mov(longword, assembly.Immediate(0), dst),
                assembly.SetCC('E', dst),
            ]

        op = self.convert_unary_operator(instr.unary_operator)
        return [
            assembly.Mov(a_type, src, dst),
            assembly.Unary(op, a_type, dst),
        ]

    def convert_unary_operator(self, op: tacky.UnaryOp) -> assembly.UnaryOperator:
        match op:
            case tacky.UnaryNegate():
                return assembly.Neg()
            case tacky.UnaryInvert():
                return assembly.Not()
            case _:
                raise Exception(f'unhandled unary op type {op}')

    def gen_binary(self, instr: tacky.Binary) -> list:
        left = self.convert_operand(instr.left)
        right = self.convert_operand(instr.right)
        dst = self.convert_operand(instr.dst)

        a_type = self.a_type_of(instr.left)
        is_signed = self.is_value_signed(instr.left)
        is_double = a_type == double

        match instr.operator:
            case tacky.BinaryAdd() | tacky.BinarySubtract() | tacky.BinaryMultiply():
                op = self.convert_binary_operator(instr.operator, is_signed)
                return [
                    assembly.Mov(a_type, left, dst),
                    assembly.Binary(op, a_type, right, dst),
                ]
            case tacky.BitOr() | tacky.BitXor() | tacky.BitAnd():
                op = self.convert_binary_operator(instr.operator, is_signed)
                return [
                    assembly.Mov(a_type, left, dst),
                    assembly.Binary(op, a_type, right, dst),
                ]
            case tacky.ShiftLeft() | tacky.ShiftRight():
                op = self.convert_binary_operator(instr.operator, is_signed)
                return [
                    assembly.Mov(a_type, left, dst),
                    assembly.Mov(a_type, right, assembly.Register('CX')),
                    assembly.Binary(op, a_type, assembly.Register('CX'), dst),
                ]
            case tacky.BinaryDivide():
                return self.generate_div(left, right, dst, a_type, False, is_signed)
            case tacky.BinaryRemainder():
                return self.generate_div(left, right, dst, a_type, True, is_signed)
            case tacky.Less() | tacky.LessEqual() | tacky.Equals() | \
                 tacky.NotEquals() | tacky.Greater() | tacky.GreaterEqual():
                comparison = self.convert_comparison(instr.operator, is_signed, is_double)
                if a_type == double and instr.operator == tacky.NotEquals():
                    # If either value is NaN, != evaluates to true
                    return [
                        assembly.Cmp(a_type, right, left),
                        assembly.Mov(longword, assembly.Immediate(0), dst),
                        assembly.Mov(longword, assembly.Immediate(0), rdx),
                        assembly.SetCC('P', rdx),
                        assembly.SetCC(comparison, dst),
                        assembly.Binary(assembly.BitOr(), longword, rdx, dst),
                    ]
                if a_type == double:
                    # If either value is NaN, all other comparisons evaluate to false
                    return [
                        assembly.Cmp(a_type, right, left),
                        assembly.Mov(longword, assembly.Immediate(0), dst),
                        assembly.Mov(longword, assembly.Immediate(0), rdx),
                        # Check the parity bit to see if either of the values was NaN
                        assembly.SetCC('NP', rdx),
                        assembly.SetCC(comparison, dst),
                        assembly.Binary(assembly.BitAnd(), longword, rdx, dst),
                    ]
                return [
                    assembly.Cmp(a_type, right, left),
                    assembly.Mov(longword, assembly.Immediate(0), dst),
                    assembly.SetCC(comparison, dst),
                ]
            case _:
                raise Exception(f'unhandled binary expression op {instr.operator}')

    def generate_div(self, left, right, dst, a_type, is_remainder, is_signed):
        if a_type == double:
            assert(not is_remainder)
            return [
                assembly.Mov(double, left, dst),
                assembly.Binary(assembly.DivDouble(), double, right, dst),
            ]

        ax = assembly.Register('AX')
        dx = assembly.Register('DX')
        instructions = [assembly.Mov(a_type, left, ax)]

        if is_signed:
            instructions.append(assembly.Cdq(a_type))
            instructions.append(assembly.Idiv(a_type, right))
        else:
            zero = assembly.Immediate(0)
            instructions.append(assembly.Mov(a_type, zero, dx))
            instructions.append(assembly.Div(a_type, right))

        if is_remainder:
            instructions.append(assembly.Mov(a_type, dx, dst))
        else:
            instructions.append(assembly.Mov(a_type, ax, dst))

        return instructions

    def a_type_of(self, value: tacky.Value):
        return self.sym_type_to_a_type(self.value_type(value))

    def is_value_signed(self, value: tacky.Value):
        return typeconversion.is_signed(self.value_type(value))

    def value_type(self, value: tacky.Value) -> syntax.Type:
        match value:
            case tacky.Constant(tacky.ConstChar(value)):
                return syntax.Char()
            case tacky.Constant(tacky.ConstUChar(value)):
                return syntax.UChar()
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

    def sym_type_to_a_type(self, sym_type: syntax.Type) -> assembly.AssemblyType:
        match sym_type:
            case syntax.Char() | syntax.UChar() | syntax.SChar():
                return byte
            case syntax.Int() | syntax.UInt():
                return longword
            case syntax.Long() | syntax.ULong() | syntax.Pointer():
                return quadword
            case syntax.Double():
                return double
            case syntax.Array(elem_t, size):
                bytes = typeconversion.type_size(elem_t, self.types) * size
                alignment = typeconversion.alignment_of(sym_type, self.types)
                return assembly.ByteArray(bytes, alignment)
            case syntax.Struct(tag):
                struct_def = self.types.get(tag)
                if struct_def is not None:
                    return assembly.ByteArray(struct_def.size, struct_def.alignment)
                else:
                    # Return a dummy value for incomplete structure declarations
                    return assembly.ByteArray(8, 8)
            case syntax.Union(tag):
                union_def = self.types.get(tag)
                if union_def is not None:
                    return assembly.ByteArray(union_def.size, union_def.alignment)
                else:
                    return assembly.ByteArray(8, 8) # dummy value
            case _:
                raise Exception(f'unexpected type {sym_type}')

    def convert_comparison(self, op: tacky.BinaryOp, is_signed: bool, is_double: bool) -> str:
        # L/LE/G/GE are used for signed integers,
        # unsigned integers and doubles use B/BE/A/AE.
        use_less_greater = is_signed and not is_double
        match op:
            case tacky.Less() if use_less_greater:
                return 'L'
            case tacky.Less():
                return 'B'
            case tacky.LessEqual() if use_less_greater:
                return 'LE'
            case tacky.LessEqual():
                return 'BE'
            case tacky.Equals():
                return 'E'
            case tacky.NotEquals():
                return 'NE'
            case tacky.GreaterEqual() if use_less_greater:
                return 'GE'
            case tacky.GreaterEqual():
                return 'AE'
            case tacky.Greater() if use_less_greater:
                return 'G'
            case tacky.Greater():
                return 'A'
            case _:
                raise Exception(f'invalid op to convert to a comparison {op}')

    def convert_binary_operator(self, op: tacky.BinaryOp, is_signed: bool) -> assembly.BinaryOperator:
        match op:
            case tacky.BinaryAdd():
                return assembly.Add()
            case tacky.BinarySubtract():
                return assembly.Sub()
            case tacky.BinaryMultiply():
                return assembly.Mult()
            case tacky.BitOr():
                return assembly.BitOr()
            case tacky.BitAnd():
                return assembly.BitAnd()
            case tacky.BitXor():
                return assembly.BitXor()
            case tacky.ShiftLeft():
                return assembly.ShiftLeft()
            case tacky.ShiftRight() if is_signed:
                return assembly.ShiftRight()
            case tacky.ShiftRight():
                return assembly.ShiftRightLogical()
            case _:
                raise Exception(f'invalid op to convert to assembly binary op {op}')

    def convert_operand(self, value: tacky.Value) -> assembly.Operand:
        value_type = self.value_type(value)
        match value:
            case tacky.Constant(tacky.ConstDouble(value)):
                label = self.add_double(value)
                return assembly.Data(label, 0)
            case tacky.Constant(const):
                return assembly.Immediate(const.value)
            case tacky.Identifier(name):
                match value_type:
                    case syntax.Array():
                        return assembly.PseudoMem(name, 0)
                    case syntax.Struct():
                        return assembly.PseudoMem(name, 0)
                    case syntax.Union():
                        return assembly.PseudoMem(name, 0)
                    case _:
                        return assembly.Pseudo(name)
            case _:
                raise Exception(f'unhandled operand type {value}')

    def add_double(self, value: float, alignment=8):
        # Packing the bytes helps distinguish 0.0 from -0.0
        packed = struct.pack('d', value)
        if packed not in self._doubles:
            label = self.new_const_label('double')
            self._doubles[packed] = (value, label, alignment)

            a_type = double
            self.asm_symbols[label] = assembly.ObjEntry(a_type, True, True)
        if self._doubles[packed][2] < alignment:
            (value, label, _prev_alignment) = self._doubles[packed]
            self._doubles[packed] = (value, label, alignment)
        return self._doubles[packed][1]


def is_mem(operand: assembly.Operand):
    match operand:
        case assembly.Memory():
            return True
        case assembly.Data():
            return True
        case assembly.Indexed():
            return True
        case _:
            return False


def is_immediate(operand: assembly.Operand):
    match operand:
        case assembly.Immediate():
            return True
        case _:
            return False


def is_large_imm(operand: assembly.Operand):
    match operand:
        case assembly.Immediate(n):
            return abs(n) > (2**31 - 1)
        case _:
            return False


def is_register(operand: assembly.Operand):
    match operand:
        case assembly.Register():
            return True
        case _:
            return False


class StackMap:
    '''This is used to figure out where each pseudo register gets stored in a
    function's stack frame'''
    def __init__(self, asm_symbols, return_in_memory):
        self.asm_symbols = asm_symbols
        self.size_used = 0
        if return_in_memory:
            # Reserve space for the pointer to the return value that is saved to -8(%rbp)
            self.size_used += 8
        self.pseudo_registers = {}

    def convert_pseudo_register(self, operand: assembly.Operand):
        match operand:
            case assembly.Pseudo(name):
                if name in self.pseudo_registers:
                    return assembly.Memory('BP', self.pseudo_registers[name])

                entry = self.asm_symbols[name]
                assert(isinstance(entry, assembly.ObjEntry))

                if entry.is_static:
                    return assembly.Data(name, 0)

                location = self._get_next_location(entry)
                self.pseudo_registers[name] = location
                return assembly.Memory('BP', location)
            case assembly.PseudoMem(name, offset):
                if name in self.pseudo_registers:
                    location = self.pseudo_registers[name]
                    return assembly.Memory('BP', location + offset)

                entry = self.asm_symbols[name]
                assert(isinstance(entry, assembly.ObjEntry))

                if entry.is_static:
                    return assembly.Data(name, offset)

                location = self._get_next_location(entry)
                self.pseudo_registers[name] = location
                return assembly.Memory('BP', location + offset)
            case _:
                return operand

    def _get_next_location(self, entry: assembly.ObjEntry):
        size = entry.assembly_type.bytes()
        match entry.assembly_type:
            case assembly.ByteArray():
                alignment = entry.assembly_type.alignment
            case _:
                alignment = size

        self.size_used += size
        # Pad for alignment
        if self.size_used % alignment != 0:
            self.size_used += alignment - (self.size_used % alignment)
        return -1 * self.size_used


@dataclass
class MemUse:
    int_seen: bool
    double_seen: bool

    def __init__(self):
        self.int_seen = False
        self.double_seen = False

    def to_mem_type(self):
        if self.double_seen and not self.int_seen:
            return MemClass.SSE
        else:
            return MemClass.INTEGER


class MemClass(Enum):
    INTEGER = 1
    SSE = 2
    MEMORY = 3


def is_scalar(type: syntax.Type) -> bool:
    match type:
        case syntax.Void():
            return False
        case syntax.Array():
            return False
        case syntax.Func():
            return False
        case syntax.Struct():
            return False
        case syntax.Union():
            return False
        case _:
            return True

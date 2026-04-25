# Compiler Review — Major Bugs

## High-impact bugs (will crash or silently corrupt)

**1. `codegen.py:1019-1021` — `gen_unary` for `UnaryNot` writes wrong number of bytes to destination**
```python
return [
    assembly.Cmp(a_type, assembly.Immediate(0), src),
    assembly.Mov(a_type, assembly.Immediate(0), dst),  # wrong a_type
    assembly.SetCC('E', dst),
]
```
`a_type` is the source's type, but `dst` always has type `Int` (per typecheck at validator.py:1349). For `!long_var` or `!ptr`, this issues `movq $0, dst`, writing 8 bytes into a 4-byte slot — corrupting adjacent stack memory. For `!char_var`, it writes only 1 byte, leaving the upper 3 bytes of the int destination uninitialized. Should use `self.a_type_of(instr.dst)` for the Mov, like the double branch already does on line 1009.

**2. `to_ir.py:469-473 / 514` — `++/--` of a dereferenced pointer ignores pointer stride**
The `PlainOperand` branch correctly handles `Pointer` by using `stride` (lines 443-448), but the `DereferencedPointer` branch just uses `make_constant_of_type(1, expr.expr_type)`. For `int **x; ++(*x);` the inner `*x` is `int*`, so the increment should advance by `sizeof(int)=4`, but this code adds 1. Same bug in the postfix path at line 514.

**3. `validator.py:1379` — `NameError` on assignment to function-typed lvalue**
```python
if isinstance(left_type, syntax.Func):
    self.error(f'Cannot assign to a function {name}')
```
`name` is undefined in this scope — this will raise NameError instead of producing a proper error message.

**4. `validator.py:1876` — `NameError` in `validate_non_void_type` for plain `Void`**
```python
case syntax.Void():
    self.error(f'Illegal array of incomplete type {elem_type}')
```
`elem_type` is undefined; the message also doesn't match the situation. Triggers when validating a void-typed function parameter etc.

**5. `validator.py:187` — `case syntax.FuncDeclaration(name, _, body, storage_class):`**
`FuncDeclaration` has 5 fields (`name`, `params`, `body`, `fun_type`, `storage_class`). Positional patterns bind to `__match_args__` in order, so the variable named `storage_class` is actually bound to `fun_type`, and the real storage class is never read. The subsequent `if storage_class == syntax.Static():` always fails. The check is luckily duplicated in `typecheck_func_decl(in_block=True)`, but as-written this branch is dead.

**6. `typeconversion.py:_constant_to_size` — breaks on negative integers**
```python
b = n.to_bytes(8, byteorder='little', signed=False)
```
Raises `OverflowError` for negative `n`. The author flagged this in a comment ("This might not be right for large longs"). Use `signed=(n<0)` (and re-decode with `signed=not unsigned`) or `n & ((1<<(n_bytes*8))-1)`.

## Bugs in the work-in-progress struct codegen

(These are clearly the unfinished area — flagging in case they bite you when you turn the code on.)

**7. `codegen.py:658 / 678` — `load_byte_array` and `store_byte_array` use undefined variables**
```python
def load_byte_array(self, a_type, src_ptr, dst):
    src = self.convert_operand(src)   # `src` undefined; param is `src_ptr`
    dst = self.convert_operand(dst)
```
```python
def store_byte_array(self, a_type, src, dst_ptr):
    src = self.convert_operand(src)
    dst = self.convert_operand(dst)   # `dst` undefined; param is `dst_ptr`
```
Both raise NameError on first call. `load_byte_array` should also assert on `dst` being PseudoMem only after assigning it.

**8. `codegen.py:859-890` — `classify_structure` references undefined `struct_type`**
The parameter is `struct_entry`, but the body uses `struct_type.size` four times. NameError.

**9. `codegen.py:898-910` — `flatten_type` matches against `assembly.Array` / `assembly.Struct`**
Those names don't exist in the `assembly` module — they're `syntax.Array` and `syntax.Struct`. Match raises `AttributeError` as soon as it's evaluated.

**10. `typeconversion.py:47` and `codegen.py:1170` — `type_size` not threaded with `types`**
`alignment_of` for arrays calls `type_size(var_type)` without `types`; `sym_type_to_a_type` for `Array` calls `typeconversion.type_size(elem_t)` without `types`. Both will crash on arrays of structs (e.g. `struct point pts[10];`).

## Minor/latent issues

- **`emit.py:72`** — `if inits == [0]:` compares a list of `StaticInit` objects to `[0]`, never true. The intended bss-section optimization for zero-initialized statics never triggers. Harmless (data section still works).
- **`codegen.py:101`** — same issue: `if var.var_type == syntax.Double() and inits[0] == 0:` compares e.g. `ZeroInit(8)` to `0`. Special case for tentative `0.0` doubles never triggers.
- **`emit.py:328`** — `case assembly.Data(name):` discards the offset; latent only because every current call site uses offset `0`.
- **`to_ir.py:596`** — typo `def convert_dot(self, expr: sytnax.Dot):` (annotation only — `from __future__ import annotations` keeps it inert).
- **`to_ir.py:482`** — `case syntax.Postfix(expr, operator):` shadows the outer `expr`; the body then uses `expr.expr_type` referring to the operand, which happens to coincide but is fragile/misleading.
- **`lexer.py:16`** — `MULTILINE_COMENT = re.compile(r'/\*.*?\*/')` has no `re.DOTALL`, so multi-line comments don't match. Hidden because `gcc -E -P` strips comments first.
- **`lexer.py:9`** — `INT_CONSTANT` regex requires a trailing non-word, non-dot character; an integer at EOF wouldn't lex (preprocessor's trailing newline saves it).
- **`syntax.py:62`** — `class Long(Type, namedtuple('Type', []))` uses typename `'Type'` instead of `'Long'`. Cosmetic only.
- **`codegen.py:1138-1157`** — `value_type` returns `syntax.Char()` for `tacky.ConstChar`; `make_constant_of_type` for `UChar` returns `tacky.ConstChar` (line 53 has a "probably works" comment). Rounding through `Char` could mis-handle signedness in `convert_operand`/`is_value_signed` when the upstream wanted `UChar`.
- **`to_ir.py:762`** — pointer-difference divide builds `tacky.Constant(tacky.ConstInt(scale))` but the result is `Long`, mixing types in the divide. Works when scale is small but is an inconsistency.

The single fix I'd prioritize is **bug #1** (`UnaryNot` byte-size mismatch) — it's the only one that silently miscompiles plausible everyday code (`!ptr`, `!long_var`).

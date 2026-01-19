This implements the extra credit from the compiler.

At the time of writing, this completes chapter 11.

# Running

`./compiler.sh <foo.c>`

flags:

- To control the stage, one of `--lex`, `--parse`, `--validate`, `--tacky`, `--codegen`, or `--emit`.
- To print the results of a stage, add `--print`.
- To create an object file instead of an executable, `-c`

# Testing

./test.sh

This assumes that [the tests](https://github.com/nlsandler/writing-a-c-compiler-tests/)
are checked out at `../writing-a-c-compiler-tests/`.

This passes along flags to the test command in that directory, such as
`--failfast`, `--latest-only`, `--stage lex`, or `--extra-credit`.


# GDB notes

To get gdb to remember history, add `set history save on` to ~/.gdbinit


Example commands to start a program:

    layout reg
    layout asm
    break main
    run

Stepping one instruction

    si

Example of printing a value in the stack (at `-0x24(%rbp)`):

    print *(int*)($rbp - 0x24)


# binq - binary queries

`binq` is an experiment in searching for patterns in binaries. It attempts to
search for expressions in the original source code by disassembling the binary
to a higher-level intermediate language, then looking for code in the
intermediate language that could conceivably have been compiled from source code
that looks like the pattern.

For example:

```nasm
xor eax, eax
```

would match both `ANY ^ ANY` and `0`, as `eax = 0` is frequently compiled to
`xor eax, eax`.

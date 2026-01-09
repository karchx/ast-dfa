# lexer-dfa

### Formal model
```
δ : Q × Σ → Q
```

## VM definition

A VM is defined as a tuple `S = (PC, Stack, Locals, Heap)`:

- OPCode this is `Σ`


```
Expr (AST)
  ↓ compile
[OPCode]
  ↓ run
VMState → VMState → …
```

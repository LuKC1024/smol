# The SMoL Language and Misconceptions

This package provides the following `#lang`s:

- `smol`: the standard model
- `dyn-scope`: function bodies are evaluated within the callee's environment
- `flat-scope`: there is only one, global, environment
- `allow-redefine`: redefinition in the same scope is allowed and treated as mutation
- `call-by-reference`: variables denote references to values rather than values themselves
- `call-by-deep-copies`: structures (in our case, only vectors) are copied when bound to variables
- `fun-copy-env`: function copies environment

We need a fancier test system because we want to simulate evaluating whole programs such that
we can error on, for example,
```racket
(defvar x 2)
(defvar x 3)
```

We need `let*-style` definitions~
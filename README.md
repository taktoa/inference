# infer

An experimental general-purpose framework for building type inference engines
for languages with type systems based on Martin-Löf type theory.

Essentially, you give it a type `U` of universes, a type `K` of constants, a
type `V` of free variables, and preorder (subtyping) relations `(∈ᵤ)`/`(∈ₖ)`
over `U` and `K` respectively.

In return, this library will give you an AST with the following properties:
- The AST uses de Bruijn indices for bound variables.
- The AST is compatible with ([higher-order][hrecursion-schemes]) recursion
  schemes, i.e.: it is defined to be the fixed point of type `ASTF` which has
  an instance for the `HFunctor` typeclass.
- Each variable is annotated with `Text` corresponding to its name before
  it was converted to de Bruijn form.
- The AST can either be open or closed, depending on the free variable type `V`,
  since if `V ≡ Void` then free variables cannot be constructed.
- Each constructor for the AST returns a value that has a type-level annotation
  of the syntactic category in which the value lies (terms versus eliminators);
  this is accomplished with GADTs.

Additionally, we have defined the following algorithms over this AST type:
- Substitution and evaluation algorithms.
- A bidirectional type inference algorithm.
- A pretty-printer that uses only ASCII.
- A pretty-printer that uses Unicode.
- A pretty-printer that uses [`ansi-wl-pprint`][ansi-wl-pprint].

FIXME: finish writing this

[hrecursion-schemes]: https://github.com/mckeankylej/hrecursion-schemes
[ansi-wl-pprint]:     https://hackage.haskell.org/package/ansi-wl-pprint

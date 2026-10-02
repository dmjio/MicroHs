# Two-level type theory in MicroHs

This branch adds staged compilation to MicroHs in the style of
András Kovács' two-level type theory (2LTT):

* *Staged Compilation with Two-Level Type Theory*, ICFP 2022
  ([demo implementation](https://github.com/AndrasKovacs/staged/tree/main/demo))
* *Closure-Free Functional Programming in a Two-Level Type Theory*, ICFP 2024

MicroHs is not dependently typed, so this is the Haskell 98 fragment of 2LTT:
two stages, a `Code` type connecting them, and the rule that every type and
term former stays within one stage.

## The language

There are two stages:

* the **meta level** (compile time, stage 1 in the papers), and
* the **object level** (run time, stage 0).

Three constructs connect the stages (the `Staged` module exports `Code`):

```haskell
Code a      -- the meta level type of object level code of type a   (⇑A in the papers)
[| e |]     -- quotation: if e :: a at the object level then [| e |] :: Code a at the meta level
~e          -- splice:    if e :: Code a at the meta level then ~e :: a at the object level
```

with the definitional equalities `~[| e |] = e` and `[| ~e |] = e`.
A splice binds tighter than application: `~f x` is `(~f) x`.

Everything else is ordinary Haskell.  The stage of a term is determined by its
type: a type that mentions `Code` is a meta level type; `IO`, `IORef`, `Ptr`,
`MVar` and the other runtime system types are object level; all other types are
*stage polymorphic* and can be used at either stage.  Likewise a top level
definition whose body uses a quotation is meta level, one that uses a splice is
object level, and anything else (including the entire Prelude) is stage
polymorphic.

```haskell
import Staged

power :: Int -> Code Int -> Code Int          -- meta level: runs in the compiler
power 0 _ = [| 1 |]
power n x = [| ~x * ~(power (n - 1) x) |]

cube :: Int -> Int                            -- object level: ordinary run time code
cube x = ~(power 3 [| x |])                   -- compiles to  cube x = x * (x * (x * 1))
```

Meta level code is evaluated by the compiler, and never exists at run time;
object level code is compiled as usual after all splices have been executed.
The result of staging can be inspected with `mhs -ddump-stage`.

### Mixed stages

Because function types are homogeneous (`a -> b` has `a` and `b` at the same
stage), a meta level function cannot take or return object level values
directly; it takes and returns `Code`.  The constructs that *can* mix stages
are bindings and case analysis:

* `let x = e in b` (and `where` without guards): the binding and the body may be
  at different stages.  A meta level binding with an object level body
  evaluates `e` at compile time (as in `power5` below).  An object level
  binding with a meta level body of type `Code t` puts the binding into the
  generated code (as in `gen` below).

* `case e of alts` and `if c then t else e`: the scrutinee (with the patterns
  and guards) and the branches may be at different stages.  A meta level
  scrutinee with object level branches is a compile time choice between pieces
  of code.  An object level scrutinee with meta level branches of type `Code t`
  generates a case expression.

```haskell
power5 :: Int -> Int
power5 x = let n = 2 + 3 in ~(power n [| x |])     -- n is computed by the compiler

gen :: Code a -> Gen (Code a)                       -- let insertion (from the Staged module)
gen c = Gen $ \ k -> [| let x = ~c in ~(k [| x |]) |]

safeDiv :: Code Int -> Code Int -> Code Int
safeDiv x y = [| case ~y of { 0 -> 0; d -> ~x `div` d } |]
```

The type checker makes these explicit by inserting quotations and splices,
exactly as Kovács' elaborator does for `let`:
`let n = 2 + 3 in ~e` becomes `~(let n = 2 + 3 in [| ~e |])`.

What is *not* allowed, as in 2LTT:

* cross stage persistence: a meta level value cannot be used in object code
  except through the literal serialization functions `codeInt`, `codeChar`,
  `codeString`, ... (and your own, e.g. `codeBool`);
* inspecting code: a `Code a` value is opaque;
* using an object level variable at the meta level other than quoted;
* eliminating object level data with a meta level result, e.g.
  `case (x :: object Int) of 0 -> (meta thing)`.  Use the `Gen` monad and
  generate a `case` instead.

Type class constraints are at the stage of their type: `Show a => Code a -> Code String`
is rejected (`a` would be at both stages); write `Code (a -> String) -> Code a -> Code String`
or use a concrete type.  Overloaded operations on concrete types work at both stages,
e.g. `show (sum [1..100 :: Int])` can be computed at compile time.

### Library

`lib/Staged.hs` provides `Code`, the serialization functions `codeInt`, `codeWord`,
`codeChar`, `codeDouble`, `codeFloat`, `codeString`, `codeBool`, and the code
generation monad `Gen` with `runGen`, `gen` (let insertion) and `genLet`.

## Implementation

### Type checker (`src/MicroHs/TypeCheck.hs`)

Stages are tracked by a small unification problem on *levels*
(`Level` in `Expr.hs`: `LMeta`, `LObj`, level variables, and `LPoly` in tables),
kept separately from types.  The design follows the observation that in 2LTT
the stage of a term is a function of its type, and all subterms of a term have
the same stage except under quote/splice, so a single level per term suffices.

* `curLevel` in the type checker state is the stage of the expression being
  checked.  A quotation requires `LMeta` and checks its body at `LObj`; a splice
  requires `LObj` and checks its body at `LMeta`.
* Every local variable records the level at which it was bound
  (`localLevels`); every top level definition has a level in `levelTable`
  (exported through `GlobTables.gLevels`).  Using a variable unifies its level
  with the current level.  A definition whose level is unconstrained after
  checking is generalized to `LPoly` and can be used at any stage.
  Definitions are checked in dependency order (`tcDefsSCC`) so that this is
  known before use.
* Type constructors and classes have *stage signatures* (`typeLevels`,
  `gTypeLevels`): the stage of `T a1 .. an` and the stages of the arguments.
  `Code :: (meta, [object])`, runtime types are object level, user data types
  are inferred from their constructor fields (`addTypeLevels`) and are
  polymorphic when unconstrained.  Written types (signatures, annotations,
  instance heads) are checked against the current level with `tcTypeLevel`.
* `let`, `case` and `if` get `EStaged from to e` markers; after a definition
  is checked, `zonkStage` turns them into `EQuote`/`ESplice` or removes them.
* Dictionaries: the level at which a constraint arises is recorded; a
  constraint solved by a dictionary argument must be at the argument's level,
  and uses of stage restricted instances are checked at the end of the module.

### Staging (`src/MicroHs/Stage.hs`)

After desugaring, quotations and splices are the pseudo primitives `$quote`
and `$splice`.  Following section 4 of the ICFP 2022 paper, staging uses two
evaluators over the desugared lambda terms:

* `eval1` evaluates meta level code to values (closures, numbers, strings,
  quoted code).  It interprets the desugared code of the current module, the
  compiled code of imported modules (the combinators `S`, `K`, `B`, ... and the
  arithmetic primitives are implemented in `primVal`), and the meta level
  definitions of imported modules, which are kept in lambda form in
  `tMetaDefs`.
* `eval0` traverses object level code, renaming binders (HOAS closures, read
  back with de Bruijn levels so that generated code never captures variables)
  and executing splices.

Meta level definitions are removed from the generated program.  Everything
is lazy, so compile time computation has ordinary Haskell semantics; a
compile time `error` or an unsupported primitive (FFI, IO) is reported as a
staging error.

### Files touched

`Expr.hs` (`EQuote`, `ESplice`, `EStaged`, `Level`), `Parse.hs`,
`TCMonad.hs`, `TypeCheck.hs`, `Desugar.hs`, `Stage.hs` (new), `Compile.hs`,
`Flags.hs` (`-ddump-stage`), `lib/Primitives.hs` (`Code`), `lib/Staged.hs` (new),
`tests/Staged*.hs`.

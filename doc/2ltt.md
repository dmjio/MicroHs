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
type: a type that mentions `Code` is a meta level type; all other types
(including `IO`, `Ptr` and the other runtime system types) are
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

Meta level code is run by the compiler, and never exists at run time;
object level code is compiled as usual after all splices have been executed.
The compiler runs meta level code with the ordinary MicroHs runtime system (the
same way the interactive system runs code), so compile time code behaves exactly
like run time code and can use all of the library, including the parts that
use the FFI (`Integer`, `show` for `Double`, ...).  It follows that splices
need a compiler that is itself compiled with MicroHs; the GHC compiled
compiler (`gmhs`) reports an error for a module with a splice.
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
`codeChar`, `codeDouble`, `codeFloat`, `codeString`, `codeBool`, `codeInteger`,
`codeList` (the code for a list, from the code of its elements), and the code
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
  `Code :: (meta, [object])`, user data types are inferred from their
  constructor fields (`addTypeLevels`) and are polymorphic when unconstrained.
  Foreign imports are stage polymorphic, except `foreign import javascript`,
  which is object level (JavaScript is not there in the compiler).  Written types (signatures, annotations,
  instance heads) are checked against the current level with `tcTypeLevel`.
* `let`, `case` and `if` get `EStaged from to e` markers; after a definition
  is checked, `zonkStage` turns them into `EQuote`/`ESplice` or removes them.
* Dictionaries: the level at which a constraint arises is recorded; a
  constraint solved by a dictionary argument must be at the argument's level,
  and uses of stage restricted instances are checked at the end of the module.

### Staging (`src/MicroHs/Stage.hs`)

After desugaring, quotations and splices are the pseudo primitives `$quote`
and `$splice`.  Staging follows section 4 of the ICFP 2022 paper, which has
two evaluators.  The meta level evaluator is the runtime system; only the
object level one is in the compiler.

* Meta level code is compiled to combinators and loaded into the running
  runtime system with `MicroHs.Translate`, like the interactive system does.
  A quotation is compiled (`quoteExp`) to code that builds a value of the type
  `Code` in `Stage.hs`: applications, global variables, literals, and binders
  as functions (HOAS).  There is no library definition of that type; the
  constructor functions are generated (`codeConstrs`) with the data type
  encoding the compiler itself uses, so the compiler can use the value that
  the runtime computes directly.  `Staged.codeInt` etc. are these constructors.
* The meta level definitions of a module are kept, as combinators, in
  `tMetaDefs`, for the splices of the modules that import it.
* The splices of a module are taken out of its object level definitions
  (`prepExp`): splice number n becomes a meta level function of the object
  level variables it uses, and all of them are loaded together, with the
  imported modules and the splice free definitions of the module itself.
  An object level variable that is `let` bound to an expression that does not
  depend on the object level can also be used inside the splice at the meta
  level; this is what the dictionary bindings of the type checker need.
* `eval0` traverses the object level code, with the binders as HOAS closures.
  At a splice it applies the splice function to the code of its variables.
  The result is read back (`quote0`) with de Bruijn levels, so that generated
  code never captures variables.

Meta level definitions are removed from the generated program.  An exception
in compile time code (`error`, division by zero, ...) is reported as an error
at the definition with the splice.  Since the splices are loaded with all the
code they can reach, the real modules of the `.hs-boot` modules are compiled
before the first module with a splice is staged.

Compile time code runs on the machine of the compiler, so with a cross
compiling target (e.g., emscripten) `Int` has the size of the host, and a
foreign function that is not in the runtime system of the compiler (one from
the user's own C code) cannot be called at compile time.

### Files touched

`Expr.hs` (`EQuote`, `ESplice`, `EStaged`, `Level`), `Parse.hs`,
`TCMonad.hs`, `TypeCheck.hs`, `Desugar.hs`, `Stage.hs` (new), `Compile.hs`,
`Flags.hs` (`-ddump-stage`), `lib/Primitives.hs` (`Code`), `lib/Staged.hs` (new),
`tests/Staged*.hs`, `tests/stagederr.test`, `tests/istaged.in`.

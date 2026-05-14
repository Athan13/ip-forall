# Interactive Prover Forall (ip-forall)

Implementing [pi-forall](https://github.com/sweirich/pi-forall).

Written in Haskell using `cabal`. To test the examples in `Main.hs`, simply run `cabal build` and `cabal run` in the directory.

The structure is as follows:

```bash
├── app
    ├── Environment.hs  // contains the typechecking monad
    ├── Main.hs         // entry point: runs `inferType` on the examples listed
    ├── Syntax.hs       // specifies the AST
    └── Typecheck.hs    // implements `inferType` and `checkType`
```

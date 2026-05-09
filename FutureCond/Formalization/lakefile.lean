import Lake
open Lake DSL

-- No external dependencies: FutureCond.lean uses only Lean 4 core (Init).
package «futurecond» where

lean_lib «FutureCond» where
  roots := #[`FutureCond]

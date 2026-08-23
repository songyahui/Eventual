import Lake
open Lake DSL

package «futurecond» where

lean_lib «PledgeMonadLaws» where
  roots := #[`PledgeMonadLaws]

lean_lib «REComposableLaws» where
  roots := #[`REComposableLaws]

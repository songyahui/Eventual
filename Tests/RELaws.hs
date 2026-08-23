-- | QuickCheck tests for the 'Composable' laws on @RE Term@.
-- Run via @cabal test pledge-laws@.
module Tests.RELaws where

import Prelude hiding ((<>))
import Control.Monad (replicateM)
import Test.QuickCheck
import Pledge

-- ── Fixed alphabet ─────────────────────────────────────────────────────────────

-- | Small fixed alphabet used for language-equality testing and RE generation.
testAlph :: [Event Term]
testAlph = [Atom "a" (List []), Atom "b" (List []), Atom "c" (List [])]

-- ── Language equality ──────────────────────────────────────────────────────────

-- | Test two REs for language equality over all words up to length 3.
-- Structural 'Eq' is insufficient: @Seq (Seq a b) c /= Seq a (Seq b c)@
-- structurally, but the two accept the same language.
langEq :: RE Term -> RE Term -> Bool
langEq r1 r2 = all (\w -> run r1 w == run r2 w) testWords
  where
    testWords    = concatMap (`replicateM` testAlph) [0 .. 3]
    run r []     = nullable r
    run r (e:es) = run (normalize (derivative e r)) es

-- ── Arbitrary instance ─────────────────────────────────────────────────────────

instance Arbitrary (RE Term) where
    arbitrary = sized genRE
      where
        genRE 0 = oneof [pure Bot, pure Epsilon, pure top, Single <$> elements testAlph]
        genRE n = oneof
            [ pure Bot
            , pure Epsilon
            , pure top
            , Single <$> elements testAlph
            , Seq  <$> genRE h <*> genRE h
            , Or   <$> genRE h <*> genRE h
            , And  <$> genRE h <*> genRE h
            , Star <$> genRE (n - 1)
            , Not  <$> genRE (n - 1)
            ]
          where h = n `div` 2
    shrink Bot         = []
    shrink Epsilon     = [Bot]
    shrink (Single _)  = [Bot, Epsilon]
    shrink (Not r)     = r : map Not  (shrink r)
    shrink (Star r)    = [Epsilon, r] ++ map Star (shrink r)
    shrink (Seq r1 r2) = [r1, r2] ++ [Seq r1' r2  | r1' <- shrink r1]
                                   ++ [Seq r1  r2' | r2' <- shrink r2]
    shrink (Or  r1 r2) = [r1, r2] ++ [Or  r1' r2  | r1' <- shrink r1]
                                   ++ [Or  r1  r2' | r2' <- shrink r2]
    shrink (And r1 r2) = [r1, r2] ++ [And r1' r2  | r1' <- shrink r1]
                                   ++ [And r1  r2' | r2' <- shrink r2]

-- ── Properties ─────────────────────────────────────────────────────────────────

-- | @(·)@ is associative: @(x · y) · z = x · (y · z)@.
prop_concat_assoc :: RE Term -> RE Term -> RE Term -> Property
prop_concat_assoc x y z =
    counterexample
        (  "LHS: " ++ show ((x · y) · z)
        ++ "\nRHS: " ++ show (x · (y · z))
        )
        (langEq ((x · y) · z) (x · (y · z)))

-- | @'empty'@ is a left identity for @(·)@: @empty · x = x@.
prop_concat_left_id :: RE Term -> Bool
prop_concat_left_id x = langEq (empty · x) x

-- | @'empty'@ is a right identity for @(·)@: @x · empty = x@.
prop_concat_right_id :: RE Term -> Bool
prop_concat_right_id x = langEq (x · empty) x

-- | @('/\\')@ is associative.
prop_conj_assoc :: RE Term -> RE Term -> RE Term -> Bool
prop_conj_assoc x y z = langEq ((x /\ y) /\ z) (x /\ (y /\ z))

-- | @('/\\')@ is commutative.
prop_conj_comm :: RE Term -> RE Term -> Bool
prop_conj_comm x y = langEq (x /\ y) (y /\ x)

-- | @'universe'@ is the identity for @('/\\')@: @universe /\\ x = x@.
prop_conj_id :: RE Term -> Bool
prop_conj_id x = langEq (universe /\ x) x

-- | @'empty'@ is the right zero for @('\\\\')@: @x \\\\ empty = x@.
prop_sub_right_zero :: RE Term -> Bool
prop_sub_right_zero x = langEq (x \\ empty) x

-- | @'universe'@ is stable under left-quotient: @universe \\\\ x = universe@.
prop_sub_universe :: RE Term -> Property
prop_sub_universe x =
    let u :: RE Term = universe
    in counterexample
        (  "x:   " ++ show x
        ++ "\nLHS: " ++ show (u \\ x)
        ++ "\nRHS: " ++ show u
        )
        (langEq (u \\ x) u)

-- | Sequential distribution of @('\\\\')@: @x \\\\ (a · b) = (x \\\\ b) \\\\ a@.
prop_sub_seq_dist :: RE Term -> RE Term -> RE Term -> Bool
prop_sub_seq_dist x a b = langEq (x \\ (a · b)) ((x \\ b) \\ a)

-- | @('\\\\')@ distributes over @('/\\')@: @(a /\\ b) \\\\ c = (a \\\\ c) /\\ (b \\\\ c)@.
prop_sub_conj_dist :: RE Term -> RE Term -> RE Term -> Bool
prop_sub_conj_dist a b c = langEq ((a /\ b) \\ c) ((a \\ c) /\ (b \\ c))

-- ── Test runner ────────────────────────────────────────────────────────────────

main :: IO ()
main = do
    putStrLn "── RE Composable law tests (QuickCheck) ─────────────────────"
    putStrLn "-- (·) associativity"        >> quickCheck prop_concat_assoc
    putStrLn "-- (·) left identity"        >> quickCheck prop_concat_left_id
    putStrLn "-- (·) right identity"       >> quickCheck prop_concat_right_id
    putStrLn "-- (/\\) associativity"      >> quickCheck prop_conj_assoc
    putStrLn "-- (/\\) commutativity"      >> quickCheck prop_conj_comm
    putStrLn "-- (/\\) identity"           >> quickCheck prop_conj_id
    putStrLn "-- (\\\\) right zero"        >> quickCheck prop_sub_right_zero
    putStrLn "-- (\\\\) universe residual" >> quickCheck prop_sub_universe
    putStrLn "-- (\\\\) sequential dist."  >> quickCheck prop_sub_seq_dist
    putStrLn "-- (\\\\) conjunction dist." >> quickCheck prop_sub_conj_dist

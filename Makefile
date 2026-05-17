.PHONY: check check-solver clean

GHC      = ghc
PACKAGES = -package containers \
           -package effectful \
           -package effectful-core \
           -package sbv
GHCFLAGS = -fno-code $(PACKAGES) -i.

# Type-check all modules reachable from Examples/Main.hs
check:
	$(GHC) $(GHCFLAGS) Examples/Main.hs

# Type-check the standalone solver test (uses old Solver import; may need updating)
check-solver:
	$(GHC) $(GHCFLAGS) Examples/UnitTest/SolverTest.hs

clean:
	find . \( -name "*.hi" -o -name "*.o" \) -not -path "./dist-newstyle/*" -delete

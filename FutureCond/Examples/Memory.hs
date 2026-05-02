{-# OPTIONS_GHC -i.. #-}
module Examples.Memory where
import Prelude hiding ((<>))
import qualified Control.Exception as Control (assert)
import Future

malloc :: Int -> Effectful RE ()
malloc addr = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single ("malloc", [Num addr])
    , future = finally ("free", [Num addr])
    }

free :: Int -> Effectful RE ()
free addr = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single ("free", [Num addr])
    , future = anything
    }

-- Good: malloc and free every address
good :: Effectful RE ()
good = do
    malloc 1
    malloc 2
    free 1
    free 2

-- Good: malloc n addresses in a loop, free them all
loopAllFreed :: Int -> Effectful RE ()
loopAllFreed n = do
    mapM_ malloc [1..n]
    mapM_ free  [1..n]

-- Bad: malloc 1 and 2, only free 1 — address 2 leaked
leak :: Effectful RE ()
leak = do
    malloc 1
    malloc 2
    free 1

-- Bad: malloc n addresses, free none
loopNoneFreed :: Int -> Effectful RE ()
loopNoneFreed n = do
    mapM_ malloc [1..n]

printResult :: String -> Effectful RE () -> IO ()
printResult name prog = do
    putStrLn $ "=== " ++ name ++ " ==="
    putStrLn $ "Post:   " ++ show (post prog)
    putStrLn $ "Future: " ++ show (normalize (future prog))
    putStrLn ""

test_derivative :: IO ()
test_derivative = do
    let r    = finally ("free", [Num 1])
        e    = Pos ("free", [Num 1])
        deri = normalize (derivitive e r)
    Control.assert (deri == anything) (putStrLn "Derivative test passed!")

main :: IO ()
main = do
    test_derivative
    printResult "good (malloc 1,2 then free 1,2)"  good
    printResult "loopAllFreed 3"                   (loopAllFreed 3)
    printResult "leak (malloc 1,2, free 1 only)"   leak
    printResult "loopNoneFreed 3"                  (loopNoneFreed 3)

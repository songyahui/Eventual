{-# OPTIONS_GHC -i.. #-}
module Examples.FileHandle where
import Prelude hiding ((<>))
import Future

-- Open a file: post = open(path), future = finally(close(path))
openFile :: String -> Effectful RE ()
openFile path = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single ("open", [Str path])
    , future = finally ("close", [Str path])
    }

-- Close a file: post = close(path), future = anything (obligation discharged)
closeFile :: String -> Effectful RE ()
closeFile path = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single ("close", [Str path])
    , future = anything
    }

-- Read from a file: no temporal obligation introduced
readFile' :: String -> Effectful RE ()
readFile' path = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single ("read", [Str path])
    , future = anything
    }

-- Good: open, read, close — future obligation discharged
goodProgram :: Effectful RE ()
goodProgram = do
    openFile "data.txt"
    readFile' "data.txt"
    closeFile "data.txt"

-- Bad: open two files, only close one — one future obligation remains
badProgram :: Effectful RE ()
badProgram = do
    openFile "a.txt"
    openFile "b.txt"
    closeFile "a.txt"

printResult :: String -> Effectful RE () -> IO ()
printResult name prog = do
    putStrLn $ "=== " ++ name ++ " ==="
    putStrLn $ "Post:   " ++ show (post prog)
    putStrLn $ "Future: " ++ show (normalize (future prog))
    putStrLn ""

main :: IO ()
main = do
    printResult "goodProgram" goodProgram
    printResult "badProgram"  badProgram

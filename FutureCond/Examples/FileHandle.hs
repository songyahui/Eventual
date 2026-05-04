{-# OPTIONS_GHC -i.. #-}
module Examples.FileHandle where
import Prelude hiding ((<>))
import Future

openFile :: String -> Effectful RE ()
openFile path = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "open" [Str path])
    , future = finally (Atom "close" [Str path])
    }

-- Precondition: last event was open or read (file must be open)
readFile' :: String -> Effectful RE ()
readFile' path = Effectful
    { ret    = ()
    , pre    = Or (Single (Atom "open" [Str path]))
                  (Single (Atom "read" [Str path]))
    , post   = Single (Atom "read" [Str path])
    , future = universe
    }

-- Precondition: last event was open or read (file must be open)
closeFile :: String -> Effectful RE ()
closeFile path = Effectful
    { ret    = ()
    , pre    = Or (Single (Atom "open" [Str path]))
                  (Single (Atom "read" [Str path]))
    , post   = Single (Atom "close" [Str path])
    , future = universe
    }

-- Good: open, read, close — preconditions satisfied, future discharged
goodProgram :: Effectful RE ()
goodProgram = do
    openFile "data.txt"
    readFile' "data.txt"
    closeFile "data.txt"

-- Good: open then close immediately (no read)
openThenClose :: Effectful RE ()
openThenClose = do
    openFile "log.txt"
    closeFile "log.txt"

-- Bad: open two files, only close one — future obligation for b.txt remains
leakedHandle :: Effectful RE ()
leakedHandle = do
    openFile "a.txt"
    closeFile "a.txt"
    openFile "b.txt"    -- future: close(b.txt) pending

-- Bad: read without open — precondition violated (pre = Bot)
readWithoutOpen :: Effectful RE ()
readWithoutOpen = readFile' "secret.txt"

printResult :: String -> Effectful RE () -> IO ()
printResult name prog = do
    putStrLn $ "=== " ++ name ++ " ==="
    putStrLn $ "Pre:    " ++ show (normalize (pre    prog))
    putStrLn $ "Post:   " ++ show (normalize (post   prog))
    putStrLn $ "Future: " ++ show (normalize (future prog))
    putStrLn ""

main :: IO ()
main = do
    printResult "goodProgram"     goodProgram
    printResult "openThenClose"   openThenClose
    printResult "leakedHandle"    leakedHandle
    printResult "readWithoutOpen" readWithoutOpen

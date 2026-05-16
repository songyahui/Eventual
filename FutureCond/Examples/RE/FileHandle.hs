{-# OPTIONS_GHC -i../.. #-}
module Examples.RE.FileHandle where
import Prelude hiding ((<>))
import FutureCond

openFile :: String -> FutureCond RE ()
openFile path = FutureCond
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "open" (List [Str path]))
    , future = \_ -> finally (Atom "close" (List [Str path]))
    }

-- Precondition: last event was open or read (file must be open)
readFile' :: String -> FutureCond RE ()
readFile' path = FutureCond
    { ret    = ()
    , pre    = Or (Single (Atom "open" (List [Str path])))
                  (Single (Atom "read" (List [Str path])))
    , post   = Single (Atom "read" (List [Str path]))
    , future = \_ -> universe
    }

-- Precondition: last event was open or read (file must be open)
closeFile :: String -> FutureCond RE ()
closeFile path = FutureCond
    { ret    = ()
    , pre    = Or (Single (Atom "open" (List [Str path])))
                  (Single (Atom "read" (List [Str path])))
    , post   = Single (Atom "close" (List [Str path]))
    , future = \_ -> universe
    }

-- Good: open, read, close — preconditions satisfied, future discharged
goodProgram :: FutureCond RE ()
goodProgram = do
    openFile "data.txt"
    readFile' "data.txt"
    closeFile "data.txt"

-- Good: open then close immediately (no read)
openThenClose :: FutureCond RE ()
openThenClose = do
    openFile "log.txt"
    closeFile "log.txt"

-- Bad: open two files, only close one — future obligation for b.txt remains
leakedHandle :: FutureCond RE ()
leakedHandle = do
    openFile "a.txt"
    closeFile "a.txt"
    openFile "b.txt"    -- future: close(b.txt) pending

-- Bad: read without open — precondition violated (pre = Bot)
readWithoutOpen :: FutureCond RE ()
readWithoutOpen = readFile' "secret.txt"

printResult :: String -> FutureCond RE () -> IO ()
printResult name prog = do
    putStrLn $ "=== " ++ name ++ " ==="
    putStrLn $ "Pre:    " ++ show (normalize (pre    prog))
    putStrLn $ "Post:   " ++ show (normalize (post   prog))
    putStrLn $ "Future: " ++ show (normalize (evalFuture prog))
    putStrLn ""

main :: IO ()
main = do
    printResult "goodProgram"     goodProgram
    printResult "openThenClose"   openThenClose
    printResult "leakedHandle"    leakedHandle
    printResult "readWithoutOpen" readWithoutOpen

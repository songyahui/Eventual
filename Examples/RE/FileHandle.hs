{-# OPTIONS_GHC -i../.. #-}
module Examples.RE.FileHandle where
import Prelude hiding ((<>))
import Pledge
import qualified System.IO as IO


openFilePledge :: FilePath -> IO.IOMode -> IO (Pledge RE IO.Handle)
openFilePledge fn mode = Pledge
    { ret    = IO.openFile fn mode
    , pre    = universe
    , post   = \h -> Single (Atom "open" (List [Handler h]))
    , future = \h -> finally (Atom "close" (List [Handler h]))
    }

openFile :: String -> Pledge RE ()
openFile path = Pledge
    { ret    = ()
    , pre    = universe
    , post   = const $ Single (Atom "open" (List [Str path]))
    , future = \_ -> finally (Atom "close" (List [Str path]))
    }

-- Precondition: last event was open or read (file must be open)
readFile' :: String -> Pledge RE ()
readFile' path = Pledge
    { ret    = ()
    , pre    = Or (Single (Atom "open" (List [Str path])))
                  (Single (Atom "read" (List [Str path])))
    , post   = const $ Single (Atom "read" (List [Str path]))
    , future = const universe
    }

-- Precondition: last event was open or read (file must be open)
closeFile :: String -> Pledge RE ()
closeFile path = Pledge
    { ret    = ()
    , pre    = Or (Single (Atom "open" (List [Str path])))
                  (Single (Atom "read" (List [Str path])))
    , post   = const $ Single (Atom "close" (List [Str path]))
    , future = const universe
    }

-- Good: open, read, close — preconditions satisfied, future discharged
goodProgram :: Pledge RE ()
goodProgram = do
    openFile "data.txt"
    readFile' "data.txt"
    closeFile "data.txt"

-- Good: open then close immediately (no read)
openThenClose :: Pledge RE ()
openThenClose = do
    openFile "log.txt"
    closeFile "log.txt"

-- Bad: open two files, only close one — future obligation for b.txt remains
leakedHandle :: Pledge RE ()
leakedHandle = do
    openFile "a.txt"
    closeFile "a.txt"
    openFile "b.txt"    -- future: close(b.txt) pending

-- Bad: read without open — precondition violated (pre = Bot)
readWithoutOpen :: Pledge RE ()
readWithoutOpen = readFile' "secret.txt"

printResult :: String -> Pledge RE () -> IO ()
printResult name prog = do
    putStrLn $ "=== " ++ name ++ " ==="
    putStrLn $ "Pre:    " ++ show (normalize (pre    prog))
    putStrLn $ "Post:   " ++ show (normalize (evalPost   prog))
    putStrLn $ "Future: " ++ show (normalize (evalFuture prog))
    putStrLn ""

main :: IO ()
main = do
    printResult "goodProgram"     goodProgram
    printResult "openThenClose"   openThenClose
    printResult "leakedHandle"    leakedHandle
    printResult "readWithoutOpen" readWithoutOpen

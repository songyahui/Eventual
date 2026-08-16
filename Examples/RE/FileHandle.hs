{-# OPTIONS_GHC -i../.. #-}
module Examples.RE.FileHandle where
import Prelude hiding ((<>))
import Pledge
import Control.Exception (evaluate)
import qualified System.IO as IO

type REHandler = RE IO.Handle

-- ── Primitive operations: IO wrappers with pledge contracts ───────────────────

-- Generic open (any mode): posts open(h), future requires close(h).
openFile :: FilePath -> IO.IOMode -> Pledge IO REHandler IO.Handle
openFile fn mode = Pledge $ do
    h <- IO.openFile fn mode
    return (h, universe, Single (Atom "open" h), finally (Atom "close" h))

-- Read-only open: posts open(h) · readMode(h).
openReadOnly :: FilePath -> Pledge IO REHandler IO.Handle
openReadOnly fn = Pledge $ do
    h <- IO.openFile fn IO.ReadMode
    return (h, universe,
            Seq (Single (Atom "open" h)) (Single (Atom "readMode" h)),
            finally (Atom "close" h))

-- Write-only open (truncates): posts open(h) · writeMode(h).
openWriteOnly :: FilePath -> Pledge IO REHandler IO.Handle
openWriteOnly fn = Pledge $ do
    h <- IO.openFile fn IO.WriteMode
    return (h, universe,
            Seq (Single (Atom "open" h)) (Single (Atom "writeMode" h)),
            finally (Atom "close" h))

-- Append open: posts open(h) · appendMode(h).
openAppend :: FilePath -> Pledge IO REHandler IO.Handle
openAppend fn = Pledge $ do
    h <- IO.openFile fn IO.AppendMode
    return (h, universe,
            Seq (Single (Atom "open" h)) (Single (Atom "appendMode" h)),
            finally (Atom "close" h))

-- Read from h: reads contents strictly so the handle is safe to close afterward.
-- Requires a read-mode open in the history.
readPledge :: IO.Handle -> Pledge IO REHandler String
readPledge h = Pledge $ do
    contents <- IO.hGetContents h
    _ <- evaluate (length contents)
    return (contents, previously (Atom "readMode" h), Single (Atom "read" h), universe)

-- Write str to h: actually writes; requires write- or append-mode open.
writePledge :: IO.Handle -> String -> Pledge IO REHandler ()
writePledge h str = Pledge $ do
    IO.hPutStr h str
    return ((), Or (previously (Atom "writeMode" h)) (previously (Atom "appendMode" h)),
            Single (Atom "write" h), universe)

-- Close h: actually closes; pre requires open(h); guards against double-close.
closePledge :: IO.Handle -> Pledge IO REHandler ()
closePledge h = Pledge $ do
    IO.hClose h
    return ((), previously (Atom "open" h), Single (Atom "close" h),
            noUntil (Atom "close" h) (Atom "open" h))

-- ── Example programs ──────────────────────────────────────────────────────────

-- Good: open read-only → read → close.
-- Post:   open(h) · readMode(h) · read(h) · close(h)
-- Future: noUntil(close(h), open(h))
goodReadFile :: FilePath -> Pledge IO REHandler String
goodReadFile fn = do
    h       <- openReadOnly fn
    content <- readPledge h
    _       <- closePledge h
    return content

-- Good: open write-only → write → close.
-- Post:   open(h) · writeMode(h) · write(h) · close(h)
-- Future: noUntil(close(h), open(h))
goodWriteFile :: FilePath -> Pledge IO REHandler ()
goodWriteFile fn = do
    h <- openWriteOnly fn
    _ <- writePledge h "content to write\n"
    closePledge h

-- Good: open append → write → close.
-- Post:   open(h) · appendMode(h) · write(h) · close(h)
-- Future: noUntil(close(h), open(h))
goodAppendFile :: FilePath -> Pledge IO REHandler ()
goodAppendFile fn = do
    h <- openAppend fn
    _ <- writePledge h "appended content\n"
    closePledge h

-- Good: copy src → dst — read src contents, write to dst, close both.
-- Both handle obligations are discharged.
goodCopyFile :: FilePath -> FilePath -> Pledge IO REHandler ()
goodCopyFile src dst = do
    hsrc    <- openReadOnly  src
    hdst    <- openWriteOnly dst
    content <- readPledge hsrc
    _       <- writePledge hdst content
    _       <- closePledge hsrc
    closePledge hdst

-- Good: write then append to the same file using two separate handles.
-- First handle is closed before the append handle opens the same path.
goodWriteThenAppend :: FilePath -> Pledge IO REHandler ()
goodWriteThenAppend fn = do
    hw <- openWriteOnly fn
    _  <- writePledge hw "initial content\n"
    _  <- closePledge hw
    ha <- openAppend fn
    _  <- writePledge ha "appended content\n"
    closePledge ha

-- Bad: write to a read-only handle.
-- The write spec requires previously(writeMode(h)) or previously(appendMode(h)),
-- but openReadOnly only posts readMode(h) — pre collapses to Bot.
badWriteToReadOnly :: FilePath -> Pledge IO REHandler ()
badWriteToReadOnly fn = do
    h <- openReadOnly fn
    _ <- writePledge h "attempted write"
    closePledge h

-- Bad: read from a write-only handle.
-- readPledge requires previously(readMode(h)),
-- but openWriteOnly only posts writeMode(h) — pre collapses to Bot.
badReadFromWriteOnly :: FilePath -> Pledge IO REHandler ()
badReadFromWriteOnly fn = do
    h <- openWriteOnly fn
    _ <- readPledge h
    closePledge h

-- Bad: read from stdin, pre-opened by the runtime and not tracked by any open pledge.
-- Pre: F(readMode(stdin)) — never posted, so pre is not satisfied.
badReadFromUntracked :: Pledge IO REHandler ()
badReadFromUntracked =
    let h = IO.stdin
    in Pledge $ return ((), previously (Atom "readMode" h), Single (Atom "read" h), universe)

-- Bad: open file but never close.
-- Future: F(close(h)) — obligation remains, never discharged.
badLeakedHandle :: FilePath -> Pledge IO REHandler IO.Handle
badLeakedHandle = openReadOnly

-- Bad: open two files but only close one.
-- h2's F(close(h2)) remains in the future — h2 is leaked.
badLeakOneHandle :: FilePath -> FilePath -> Pledge IO REHandler ()
badLeakOneHandle fn1 fn2 = do
    h1 <- openReadOnly fn1
    h2 <- openReadOnly fn2
    _  <- readPledge h1
    _  <- readPledge h2
    closePledge h1
    -- closePledge h2 intentionally omitted: h2 leaks

-- Bad: close a handle twice in a row.
-- After the first close, future = noUntil(close(h), open(h)).
-- The second close posts close(h) before any open(h) — future becomes ∅.
badDoubleClose :: FilePath -> Pledge IO REHandler ()
badDoubleClose fn = do
    h <- openReadOnly fn
    _ <- closePledge h
    closePledge h

-- Bad: read then attempt to write to a read-only handle.
-- writeMode(h) never posted — write pre collapses to Bot.
badWriteAfterRead :: FilePath -> Pledge IO REHandler ()
badWriteAfterRead fn = do
    h <- openReadOnly fn
    _ <- readPledge h
    _ <- writePledge h "attempted write"
    closePledge h

-- ── Main ──────────────────────────────────────────────────────────────────────

main :: IO ()
main = do
    IO.writeFile "/tmp/pledge_a.txt" "hello from pledge\n"
    IO.writeFile "/tmp/pledge_b.txt" ""
    IO.writeFile "/tmp/pledge_c.txt" "copy source\n"

    putStrLn "── Good programs (all obligations discharged) ──────────────────"

    _ <- printOfPledgeRE "goodReadFile"        (goodReadFile        "/tmp/pledge_a.txt")
    _ <- printOfPledgeRE "goodWriteFile"       (goodWriteFile       "/tmp/pledge_b.txt")
    _ <- printOfPledgeRE "goodAppendFile"      (goodAppendFile      "/tmp/pledge_b.txt")
    _ <- printOfPledgeRE "goodCopyFile"        (goodCopyFile        "/tmp/pledge_c.txt" "/tmp/pledge_b.txt")
    _ <- printOfPledgeRE "goodWriteThenAppend" (goodWriteThenAppend "/tmp/pledge_b.txt")

    putStrLn "── Bad programs (pre = Bot or future ≠ Σ*) ────────────────────"

    _ <- printOfPledgeRE "badWriteToReadOnly"   (badWriteToReadOnly   "/tmp/pledge_a.txt")
    _ <- printOfPledgeRE "badReadFromWriteOnly" (badReadFromWriteOnly  "/tmp/pledge_b.txt")
    _ <- printOfPledgeRE "badReadFromUntracked"  badReadFromUntracked
    _ <- printOfPledgeRE "badLeakedHandle"      (badLeakedHandle       "/tmp/pledge_a.txt")
    _ <- printOfPledgeRE "badLeakOneHandle"     (badLeakOneHandle      "/tmp/pledge_a.txt" "/tmp/pledge_c.txt")
    _ <- printOfPledgeRE "badDoubleClose"       (badDoubleClose        "/tmp/pledge_a.txt")
    _ <- printOfPledgeRE "badWriteAfterRead"    (badWriteAfterRead     "/tmp/pledge_a.txt")

    return ()
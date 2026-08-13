{-# OPTIONS_GHC -i../.. #-}
module Examples.RE.FileHandle where
import Prelude hiding ((<>))
import Pledge
import Control.Exception (evaluate)
import qualified System.IO as IO

type REHandler = RE IO.Handle

-- ── Primitive operations: IO wrappers with pledge contracts ───────────────────

-- Generic open (any mode): posts open(h), future requires close(h).
openFile :: FilePath -> IO.IOMode -> IO (Pledge REHandler IO.Handle)
openFile fn mode = do
    h <- IO.openFile fn mode
    return $ Pledge
        { ret    = h
        , pre    = universe
        , post   = Single (Atom "open" h)
        , future = finally (Atom "close" h)
        }

-- Read-only open: posts open(h) · readMode(h).
openReadOnly :: FilePath -> IO (Pledge REHandler IO.Handle)
openReadOnly fn = do
    h <- IO.openFile fn IO.ReadMode
    return $ Pledge
        { ret    = h
        , pre    = universe
        , post   = Seq (Single (Atom "open" h)) (Single (Atom "readMode" h))
        , future = finally (Atom "close" h)
        }

-- Write-only open (truncates): posts open(h) · writeMode(h).
openWriteOnly :: FilePath -> IO (Pledge REHandler IO.Handle)
openWriteOnly fn = do
    h <- IO.openFile fn IO.WriteMode
    return $ Pledge
        { ret    = h
        , pre    = universe
        , post   = Seq (Single (Atom "open" h)) (Single (Atom "writeMode" h))
        , future = finally (Atom "close" h)
        }

-- Append open: posts open(h) · appendMode(h).
openAppend :: FilePath -> IO (Pledge REHandler IO.Handle)
openAppend fn = do
    h <- IO.openFile fn IO.AppendMode
    return $ Pledge
        { ret    = h
        , pre    = universe
        , post   = Seq (Single (Atom "open" h)) (Single (Atom "appendMode" h))
        , future = finally (Atom "close" h)
        }

-- Read from h (IO): reads contents strictly so the handle is safe to close
-- afterward; requires a read-mode open in the history.
readPledge :: IO.Handle -> IO (Pledge REHandler String)
readPledge h = do
    contents <- IO.hGetContents h
    _ <- evaluate (length contents)  -- force full read before returning
    return $ Pledge
        { ret    = contents
        , pre    = previously (Atom "readMode" h)
        , post   = Single (Atom "read" h)
        , future = universe
        }

-- Write str to h (IO): actually writes; requires write- or append-mode open.
writePledge :: IO.Handle -> String -> IO (Pledge REHandler ())
writePledge h str = do
    IO.hPutStr h str
    return $ Pledge
        { ret    = ()
        , pre    = Or (previously (Atom "writeMode" h))
                      (previously (Atom "appendMode" h))
        , post   = Single (Atom "write" h)
        , future = universe
        }

-- Close h (IO): actually closes; pre requires open(h); guards against double-close.
closePledge :: IO.Handle -> IO (Pledge REHandler ())
closePledge h = do
    IO.hClose h
    return $ Pledge
        { ret    = ()
        , pre    = previously (Atom "open" h)
        , post   = Single (Atom "close" h)
        , future = noUntil (Atom "close" h) (Atom "open" h)
        }

-- ── Example programs ──────────────────────────────────────────────────────────

-- Good: open read-only → read → close.
-- readPledge forces the full read; closePledge then closes the handle safely.
-- Post:   open(h) · readMode(h) · read(h) · close(h)
-- Future: noUntil(close(h), open(h))
goodReadFile :: FilePath -> IO (Pledge REHandler String)
goodReadFile fn = do
    ph     <- openReadOnly fn
    let h = ret ph
    pread  <- readPledge h
    pclose <- closePledge h
    return $ do
        _       <- ph
        content <- pread
        _       <- pclose
        return content

-- Good: open write-only → write → close.
-- Post:   open(h) · writeMode(h) · write(h) · close(h)
-- Future: noUntil(close(h), open(h))
goodWriteFile :: FilePath -> IO (Pledge REHandler ())
goodWriteFile fn = do
    ph     <- openWriteOnly fn
    let h = ret ph
    pwrite <- writePledge h "content to write\n"
    pclose <- closePledge h
    return $ ph >> pwrite >> pclose

-- Good: open append → write → close.
-- Post:   open(h) · appendMode(h) · write(h) · close(h)
-- Future: noUntil(close(h), open(h))
goodAppendFile :: FilePath -> IO (Pledge REHandler ())
goodAppendFile fn = do
    ph      <- openAppend fn
    let h = ret ph
    pappend <- writePledge h "appended content\n"
    pclose  <- closePledge h
    return $ ph >> pappend >> pclose

-- Good: copy src → dst — read src contents, write to dst, close both.
-- writePledge receives the already-forced string from readPledge.
-- Both handle obligations are discharged.
goodCopyFile :: FilePath -> FilePath -> IO (Pledge REHandler ())
goodCopyFile src dst = do
    psrc    <- openReadOnly  src
    pdst    <- openWriteOnly dst
    pread   <- readPledge  (ret psrc)
    pwrite  <- writePledge (ret pdst) (ret pread)
    pclose1 <- closePledge (ret psrc)
    pclose2 <- closePledge (ret pdst)
    return $ psrc >> pdst >> pread >> pwrite >> pclose1 >> pclose2

-- Good: write then append to the same file using two separate handles.
-- First handle is closed before the append handle opens the same path.
goodWriteThenAppend :: FilePath -> IO (Pledge REHandler ())
goodWriteThenAppend fn = do
    pw      <- openWriteOnly fn
    pwrite  <- writePledge (ret pw) "initial content\n"
    pclose1 <- closePledge (ret pw)
    pa      <- openAppend fn
    pappend <- writePledge (ret pa) "appended content\n"
    pclose2 <- closePledge (ret pa)
    return $ pw >> pwrite >> pclose1 >> pa >> pappend >> pclose2

-- Bad: write to a read-only handle.
-- The write spec requires previously(writeMode(h)) or previously(appendMode(h)),
-- but openReadOnly only posts readMode(h) — pre collapses to Bot.
-- Write spec is constructed inline; IO.hPutStr is not called to avoid a runtime error.
badWriteToReadOnly :: FilePath -> IO (Pledge REHandler ())
badWriteToReadOnly fn = do
    ph <- openReadOnly fn
    let h = ret ph
    pwrite <- writePledge h "attempted write"
    pclose <- closePledge h
    return $ ph >> pwrite >> pclose

-- Bad: read from a write-only handle.
-- readPledge requires previously(readMode(h)),
-- but openWriteOnly only posts writeMode(h) — pre collapses to Bot.
-- Read spec is constructed inline; IO.hGetContents is not called.
badReadFromWriteOnly :: FilePath -> IO (Pledge REHandler ())
badReadFromWriteOnly fn = do
    ph <- openWriteOnly fn
    let h = ret ph
    pread <- readPledge h
    pclose <- closePledge h
    return $ ph >> pread >> pclose

-- Bad: read from stdin, pre-opened by the runtime and not tracked by any open pledge.
-- Pre: F(readMode(stdin)) — never posted, so pre is not satisfied.
-- Spec-only: calling readPledge IO.stdin would block waiting for terminal input.
badReadFromUntracked :: IO (Pledge REHandler ())
badReadFromUntracked =
    let h = IO.stdin
    in  return $ Pledge
            { ret    = ()
            , pre    = previously (Atom "readMode" h)
            , post   = Single (Atom "read" h)
            , future = universe
            }

-- Bad: open file but never close.
-- Future: F(close(h)) — obligation remains, never discharged.
badLeakedHandle :: FilePath -> IO (Pledge REHandler IO.Handle)
badLeakedHandle = openReadOnly

-- Bad: open two files but only close one.
-- h2's F(close(h2)) remains in the future — h2 is leaked.
-- readPledge is called for both since both are opened for reading.
badLeakOneHandle :: FilePath -> FilePath -> IO (Pledge REHandler ())
badLeakOneHandle fn1 fn2 = do
    ph1    <- openReadOnly fn1
    ph2    <- openReadOnly fn2
    pread1 <- readPledge (ret ph1)
    pread2 <- readPledge (ret ph2)
    pclose1 <- closePledge (ret ph1)
    return $ ph1 >> ph2 >> pread1 >> pread2 >> pclose1
    -- closePledge (ret ph2) intentionally omitted: h2 leaks

-- Bad: close a handle twice in a row.
-- After the first close, future = noUntil(close(h), open(h)).
-- The second close spec posts close(h) before any open(h) — future becomes ∅.
-- The second closePledge is an inline spec only; IO.hClose is not called twice.
badDoubleClose :: FilePath -> IO (Pledge REHandler ())
badDoubleClose fn = do
    ph      <- openReadOnly fn
    let h = ret ph
    pclose1 <- closePledge h
    pclose2 <- closePledge h
    return $ ph >> pclose1 >> pclose2

-- Bad: read then attempt to write to a read-only handle.
-- readPledge succeeds (handle is read-mode); write spec is inline (no IO.hPutStr).
-- writeMode(h) never posted — write pre collapses to Bot.
badWriteAfterRead :: FilePath -> IO (Pledge REHandler ())
badWriteAfterRead fn = do
    ph    <- openReadOnly fn
    let h = ret ph
    pread <- readPledge h
    pwrite <- writePledge h "attempted write"
    pclose <- closePledge h
    return $ ph >> pread >> pwrite >> pclose

-- ── Main ──────────────────────────────────────────────────────────────────────

main :: IO ()
main = do
    IO.writeFile "/tmp/pledge_a.txt" "hello from pledge\n"
    IO.writeFile "/tmp/pledge_b.txt" ""
    IO.writeFile "/tmp/pledge_c.txt" "copy source\n"

    putStrLn "── Good programs (all obligations discharged) ──────────────────"

    goodReadFile'        <- goodReadFile        "/tmp/pledge_a.txt"
    _ <- printOfPledgeRE "goodReadFile"          goodReadFile'

    goodWriteFile'       <- goodWriteFile       "/tmp/pledge_b.txt"
    _ <- printOfPledgeRE "goodWriteFile"         goodWriteFile'

    goodAppendFile'      <- goodAppendFile      "/tmp/pledge_b.txt"
    _ <- printOfPledgeRE "goodAppendFile"        goodAppendFile'

    goodCopyFile'        <- goodCopyFile        "/tmp/pledge_c.txt" "/tmp/pledge_b.txt"
    _ <- printOfPledgeRE "goodCopyFile"          goodCopyFile'

    goodWriteThenAppend' <- goodWriteThenAppend "/tmp/pledge_b.txt"
    _ <- printOfPledgeRE "goodWriteThenAppend"   goodWriteThenAppend'

    putStrLn "── Bad programs (pre = Bot or future ≠ Σ*) ────────────────────"

    badWriteToReadOnly'   <- badWriteToReadOnly   "/tmp/pledge_a.txt"
    _ <- printOfPledgeRE "badWriteToReadOnly"      badWriteToReadOnly'

    badReadFromWriteOnly' <- badReadFromWriteOnly  "/tmp/pledge_b.txt"
    _ <- printOfPledgeRE "badReadFromWriteOnly"    badReadFromWriteOnly'

    badReadFromUntracked' <- badReadFromUntracked
    _ <- printOfPledgeRE "badReadFromUntracked"    badReadFromUntracked'

    badLeakedHandle'     <- badLeakedHandle       "/tmp/pledge_a.txt"
    _ <- printOfPledgeRE "badLeakedHandle"         badLeakedHandle'

    badLeakOneHandle'    <- badLeakOneHandle      "/tmp/pledge_a.txt" "/tmp/pledge_c.txt"
    _ <- printOfPledgeRE "badLeakOneHandle"        badLeakOneHandle'

    badDoubleClose'      <- badDoubleClose        "/tmp/pledge_a.txt"
    _ <- printOfPledgeRE "badDoubleClose"          badDoubleClose'

    badWriteAfterRead'   <- badWriteAfterRead     "/tmp/pledge_a.txt"
    _ <- printOfPledgeRE "badWriteAfterRead"       badWriteAfterRead'

    return ()

{-# OPTIONS_GHC -i.. #-}
module Examples.CryptoSession where
import Prelude hiding ((<>))
import Future

initSession :: String -> Effectful RE ()
initSession sid = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "initSession" [Str sid])
    , future = finally (Atom "finalizeSession" [Str sid])
    }

finalizeSession :: String -> Effectful RE ()
finalizeSession sid = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "finalizeSession" [Str sid])
    , future = universe
    }

-- Nonce must be consumed exactly once (use-once enforcement via future)
generateNonce :: Int -> Effectful RE ()
generateNonce nid = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "generateNonce" [Num nid])
    , future = finally (Atom "consumeNonce" [Num nid])
    }

-- Precondition: nonce must have just been generated
consumeNonce :: Int -> Effectful RE ()
consumeNonce nid = Effectful
    { ret    = ()
    , pre    = Single (Atom "generateNonce" [Num nid])
    , post   = Single (Atom "consumeNonce" [Num nid])
    , future = universe
    }

encrypt :: String -> String -> Effectful RE ()
encrypt sid msg = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "encrypt" [Str sid, Str msg])
    , future = universe
    }

-- Good: session opened, nonce generated and consumed, session closed
goodHandshake :: Effectful RE ()
goodHandshake = do
    initSession "sess-1"
    generateNonce 42
    consumeNonce 42
    encrypt "sess-1" "hello"
    finalizeSession "sess-1"

-- Bad: nonce generated but never consumed (replay attack risk) — future remains
nonceLeak :: Effectful RE ()
nonceLeak = do
    initSession "sess-2"
    generateNonce 99
    encrypt "sess-2" "secret"
    finalizeSession "sess-2"

-- Bad: session never finalized — future remains
unclosedSession :: Effectful RE ()
unclosedSession = do
    initSession "sess-3"
    generateNonce 7
    consumeNonce 7
    encrypt "sess-3" "data"

printResult :: String -> Effectful RE () -> IO ()
printResult name prog = do
    putStrLn $ "=== " ++ name ++ " ==="
    putStrLn $ "Pre:    " ++ show (normalize (pre    prog))
    putStrLn $ "Post:   " ++ show (normalize (post   prog))
    putStrLn $ "Future: " ++ show (normalize (future prog))
    putStrLn ""

main :: IO ()
main = do
    printResult "goodHandshake"   goodHandshake
    printResult "nonceLeak"       nonceLeak
    printResult "unclosedSession" unclosedSession

{-# OPTIONS_GHC -i../.. #-}
module Examples.RE.CryptoSession where
import Prelude hiding ((<>))
import FutureCond

initSession :: String -> FutureCond RE ()
initSession sid = FutureCond
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "initSession" (List [Str sid]))
    , future = \_ -> finally (Atom "finalizeSession" (List [Str sid]))
    }

finalizeSession :: String -> FutureCond RE ()
finalizeSession sid = FutureCond
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "finalizeSession" (List [Str sid]))
    , future = \_ -> universe
    }

-- Nonce must be consumed exactly once (use-once enforcement via future)
generateNonce :: Int -> FutureCond RE ()
generateNonce nid = FutureCond
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "generateNonce" (List [Num nid]))
    , future = \_ -> finally (Atom "consumeNonce" (List [Num nid]))
    }

-- Precondition: nonce must have just been generated
consumeNonce :: Int -> FutureCond RE ()
consumeNonce nid = FutureCond
    { ret    = ()
    , pre    = Single (Atom "generateNonce" (List [Num nid]))
    , post   = Single (Atom "consumeNonce" (List [Num nid]))
    , future = \_ -> universe
    }

encrypt :: String -> String -> FutureCond RE ()
encrypt sid msg = FutureCond
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "encrypt" (List [Str sid, Str msg]))
    , future = \_ -> universe
    }

-- Good: session opened, nonce generated and consumed, session closed
goodHandshake :: FutureCond RE ()
goodHandshake = do
    initSession "sess-1"
    generateNonce 42
    consumeNonce 42
    encrypt "sess-1" "hello"
    finalizeSession "sess-1"

-- Bad: nonce generated but never consumed (replay attack risk) — future remains
nonceLeak :: FutureCond RE ()
nonceLeak = do
    initSession "sess-2"
    generateNonce 99
    encrypt "sess-2" "secret"
    finalizeSession "sess-2"

-- Bad: session never finalized — future remains
unclosedSession :: FutureCond RE ()
unclosedSession = do
    initSession "sess-3"
    generateNonce 7
    consumeNonce 7
    encrypt "sess-3" "data"

printResult :: String -> FutureCond RE () -> IO ()
printResult name prog = do
    putStrLn $ "=== " ++ name ++ " ==="
    putStrLn $ "Pre:    " ++ show (normalize (pre    prog))
    putStrLn $ "Post:   " ++ show (normalize (post   prog))
    putStrLn $ "Future: " ++ show (normalize (evalFuture prog))
    putStrLn ""

main :: IO ()
main = do
    printResult "goodHandshake"   goodHandshake
    printResult "nonceLeak"       nonceLeak
    printResult "unclosedSession" unclosedSession

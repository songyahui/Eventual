{-# OPTIONS_GHC -i../.. #-}
module Examples.RE.NetworkProtocol where
import Prelude hiding ((<>))
import FutureCond

-- TCP-like three-way handshake modelled as effectful steps.

sendSYN :: FutureCond RE ()
sendSYN = FutureCond
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "sendSYN" (List []))
    , future = \_ -> finally (Atom "recvSYNACK" (List []))
    }

-- Precondition: sendSYN must have just occurred
recvSYNACK :: FutureCond RE ()
recvSYNACK = FutureCond
    { ret    = ()
    , pre    = Single (Atom "sendSYN" (List []))
    , post   = Single (Atom "recvSYNACK" (List []))
    , future = \_ -> finally (Atom "sendACK" (List []))
    }

-- Precondition: recvSYNACK must have just occurred
sendACK :: FutureCond RE ()
sendACK = FutureCond
    { ret    = ()
    , pre    = Single (Atom "recvSYNACK" (List []))
    , post   = Single (Atom "sendACK" (List []))
    , future = \_ -> universe
    }

sendData :: String -> FutureCond RE ()
sendData payload = FutureCond
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "sendData" (List [Str payload]))
    , future = \_ -> universe
    }

sendFIN :: FutureCond RE ()
sendFIN = FutureCond
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "sendFIN" (List []))
    , future = \_ -> finally (Atom "recvFINACK" (List []))
    }

recvFINACK :: FutureCond RE ()
recvFINACK = FutureCond
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "recvFINACK" (List []))
    , future = \_ -> universe
    }

-- Good: complete handshake, data, teardown — all preconditions met, no future pending
fullSession :: FutureCond RE ()
fullSession = do
    sendSYN
    recvSYNACK
    sendACK
    sendData "GET / HTTP/1.1"
    sendFIN
    recvFINACK

-- Bad: SYN sent but handshake never completed — future pending
stalledHandshake :: FutureCond RE ()
stalledHandshake = do
    sendSYN

-- Bad: recvSYNACK called without sendSYN — precondition violated
outOfOrder :: FutureCond RE ()
outOfOrder = do
    recvSYNACK
    sendACK

-- Bad: connection never torn down — future pending
teardownMissed :: FutureCond RE ()
teardownMissed = do
    sendSYN
    recvSYNACK
    sendACK
    sendData "payload"
    sendFIN
    -- missing recvFINACK

printResult :: String -> FutureCond RE () -> IO ()
printResult name prog = do
    putStrLn $ "=== " ++ name ++ " ==="
    putStrLn $ "Pre:    " ++ show (normalize (pre    prog))
    putStrLn $ "Post:   " ++ show (normalize (post   prog))
    putStrLn $ "Future: " ++ show (normalize (evalFuture prog))
    putStrLn ""

main :: IO ()
main = do
    printResult "fullSession"      fullSession
    printResult "stalledHandshake" stalledHandshake
    printResult "outOfOrder"       outOfOrder
    printResult "teardownMissed"   teardownMissed

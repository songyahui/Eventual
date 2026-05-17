{-# OPTIONS_GHC -i../.. #-}
module Examples.RE.NetworkProtocol where
import Prelude hiding ((<>))
import Pledge

-- TCP-like three-way handshake modelled as effectful steps.

sendSYN :: Pledge RE ()
sendSYN = Pledge
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "sendSYN" (List []))
    , future = \_ -> finally (Atom "recvSYNACK" (List []))
    }

-- Precondition: sendSYN must have just occurred
recvSYNACK :: Pledge RE ()
recvSYNACK = Pledge
    { ret    = ()
    , pre    = Single (Atom "sendSYN" (List []))
    , post   = Single (Atom "recvSYNACK" (List []))
    , future = \_ -> finally (Atom "sendACK" (List []))
    }

-- Precondition: recvSYNACK must have just occurred
sendACK :: Pledge RE ()
sendACK = Pledge
    { ret    = ()
    , pre    = Single (Atom "recvSYNACK" (List []))
    , post   = Single (Atom "sendACK" (List []))
    , future = const universe
    }

sendData :: String -> Pledge RE ()
sendData payload = Pledge
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "sendData" (List [Str payload]))
    , future = const universe
    }

sendFIN :: Pledge RE ()
sendFIN = Pledge
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "sendFIN" (List []))
    , future = \_ -> finally (Atom "recvFINACK" (List []))
    }

recvFINACK :: Pledge RE ()
recvFINACK = Pledge
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "recvFINACK" (List []))
    , future = const universe
    }

-- Good: complete handshake, data, teardown — all preconditions met, no future pending
fullSession :: Pledge RE ()
fullSession = do
    sendSYN
    recvSYNACK
    sendACK
    sendData "GET / HTTP/1.1"
    sendFIN
    recvFINACK

-- Bad: SYN sent but handshake never completed — future pending
stalledHandshake :: Pledge RE ()
stalledHandshake = do
    sendSYN

-- Bad: recvSYNACK called without sendSYN — precondition violated
outOfOrder :: Pledge RE ()
outOfOrder = do
    recvSYNACK
    sendACK

-- Bad: connection never torn down — future pending
teardownMissed :: Pledge RE ()
teardownMissed = do
    sendSYN
    recvSYNACK
    sendACK
    sendData "payload"
    sendFIN
    -- missing recvFINACK

printResult :: String -> Pledge RE () -> IO ()
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

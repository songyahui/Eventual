{-# OPTIONS_GHC -i.. #-}
module Examples.NetworkProtocol where
import Prelude hiding ((<>))
import Future

-- TCP-like three-way handshake modelled as effectful steps.

sendSYN :: Effectful RE ()
sendSYN = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "sendSYN" [])
    , future = finally (Atom "recvSYNACK" [])
    }

-- Precondition: sendSYN must have just occurred
recvSYNACK :: Effectful RE ()
recvSYNACK = Effectful
    { ret    = ()
    , pre    = Single (Atom "sendSYN" [])
    , post   = Single (Atom "recvSYNACK" [])
    , future = finally (Atom "sendACK" [])
    }

-- Precondition: recvSYNACK must have just occurred
sendACK :: Effectful RE ()
sendACK = Effectful
    { ret    = ()
    , pre    = Single (Atom "recvSYNACK" [])
    , post   = Single (Atom "sendACK" [])
    , future = universe
    }

sendData :: String -> Effectful RE ()
sendData payload = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "sendData" [Str payload])
    , future = universe
    }

sendFIN :: Effectful RE ()
sendFIN = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "sendFIN" [])
    , future = finally (Atom "recvFINACK" [])
    }

recvFINACK :: Effectful RE ()
recvFINACK = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "recvFINACK" [])
    , future = universe
    }

-- Good: complete handshake, data, teardown — all preconditions met, no future pending
fullSession :: Effectful RE ()
fullSession = do
    sendSYN
    recvSYNACK
    sendACK
    sendData "GET / HTTP/1.1"
    sendFIN
    recvFINACK

-- Bad: SYN sent but handshake never completed — future pending
stalledHandshake :: Effectful RE ()
stalledHandshake = do
    sendSYN

-- Bad: recvSYNACK called without sendSYN — precondition violated
outOfOrder :: Effectful RE ()
outOfOrder = do
    recvSYNACK
    sendACK

-- Bad: connection never torn down — future pending
teardownMissed :: Effectful RE ()
teardownMissed = do
    sendSYN
    recvSYNACK
    sendACK
    sendData "payload"
    sendFIN
    -- missing recvFINACK

printResult :: String -> Effectful RE () -> IO ()
printResult name prog = do
    putStrLn $ "=== " ++ name ++ " ==="
    putStrLn $ "Pre:    " ++ show (normalize (pre    prog))
    putStrLn $ "Post:   " ++ show (normalize (post   prog))
    putStrLn $ "Future: " ++ show (normalize (future prog))
    putStrLn ""

main :: IO ()
main = do
    printResult "fullSession"      fullSession
    printResult "stalledHandshake" stalledHandshake
    printResult "outOfOrder"       outOfOrder
    printResult "teardownMissed"   teardownMissed

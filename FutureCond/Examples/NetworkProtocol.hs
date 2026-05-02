{-# OPTIONS_GHC -i.. #-}
module Examples.NetworkProtocol where
import Prelude hiding ((<>))
import Future

-- TCP-like three-way handshake modelled as effectful steps.
-- Each step enforces that the next expected step must eventually occur.

-- Client sends SYN: future = eventually recvSYNACK
sendSYN :: Effectful RE ()
sendSYN = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single ("sendSYN", [])
    , future = finally ("recvSYNACK", [])
    }

-- Client receives SYN-ACK: discharges sendSYN, future = eventually sendACK
recvSYNACK :: Effectful RE ()
recvSYNACK = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single ("recvSYNACK", [])
    , future = finally ("sendACK", [])
    }

-- Client sends ACK: connection established, no further obligation
sendACK :: Effectful RE ()
sendACK = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single ("sendACK", [])
    , future = anything
    }

-- Send data on an established connection: no obligation
sendData :: String -> Effectful RE ()
sendData payload = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single ("sendData", [Str payload])
    , future = anything
    }

-- Teardown: after data transfer, future = eventually recvFINACK
sendFIN :: Effectful RE ()
sendFIN = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single ("sendFIN", [])
    , future = finally ("recvFINACK", [])
    }

recvFINACK :: Effectful RE ()
recvFINACK = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single ("recvFINACK", [])
    , future = anything
    }

-- Good: complete handshake, data transfer, clean teardown
fullSession :: Effectful RE ()
fullSession = do
    sendSYN
    recvSYNACK
    sendACK
    sendData "GET / HTTP/1.1"
    sendFIN
    recvFINACK

-- Bad: handshake started but SYN-ACK never received
stalledHandshake :: Effectful RE ()
stalledHandshake = do
    sendSYN

-- Bad: handshake complete, data sent, connection never torn down
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
    putStrLn $ "Post:   " ++ show (post prog)
    putStrLn $ "Future: " ++ show (normalize (future prog))
    putStrLn ""

main :: IO ()
main = do
    printResult "fullSession"      fullSession
    printResult "stalledHandshake" stalledHandshake
    printResult "teardownMissed"   teardownMissed

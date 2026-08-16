{-# OPTIONS_GHC -i../.. #-}
module Examples.RE.NetworkProtocol where
import Prelude hiding ((<>))
import Pledge

-- TCP-like three-way handshake modelled as effectful steps.

sendSYN :: Pledge IO (RE Term) ()
sendSYN = Pledge $ return
    ((), universe,
     Single (Atom "sendSYN" (List [])),
     finally (Atom "recvSYNACK" (List [])))

-- Precondition: sendSYN must have just occurred
recvSYNACK :: Pledge IO (RE Term) ()
recvSYNACK = Pledge $ return
    ((), Single (Atom "sendSYN" (List [])),
     Single (Atom "recvSYNACK" (List [])),
     finally (Atom "sendACK" (List [])))

-- Precondition: recvSYNACK must have just occurred
sendACK :: Pledge IO (RE Term) ()
sendACK = Pledge $ return
    ((), Single (Atom "recvSYNACK" (List [])),
     Single (Atom "sendACK" (List [])),
     universe)

sendData :: String -> Pledge IO (RE Term) ()
sendData payload = Pledge $ return
    ((), universe, Single (Atom "sendData" (List [Str payload])), universe)

sendFIN :: Pledge IO (RE Term) ()
sendFIN = Pledge $ return
    ((), universe,
     Single (Atom "sendFIN" (List [])),
     finally (Atom "recvFINACK" (List [])))

recvFINACK :: Pledge IO (RE Term) ()
recvFINACK = Pledge $ return
    ((), universe, Single (Atom "recvFINACK" (List [])), universe)

-- Good: complete handshake, data, teardown — all preconditions met, no future pending
fullSession :: Pledge IO (RE Term) ()
fullSession = do
    sendSYN
    recvSYNACK
    sendACK
    sendData "GET / HTTP/1.1"
    sendFIN
    recvFINACK

-- Bad: SYN sent but handshake never completed — future pending
stalledHandshake :: Pledge IO (RE Term) ()
stalledHandshake = do
    sendSYN

-- Bad: recvSYNACK called without sendSYN — precondition violated
outOfOrder :: Pledge IO (RE Term) ()
outOfOrder = do
    recvSYNACK
    sendACK

-- Bad: connection never torn down — future pending
teardownMissed :: Pledge IO (RE Term) ()
teardownMissed = do
    sendSYN
    recvSYNACK
    sendACK
    sendData "payload"
    sendFIN
    -- missing recvFINACK

printResult :: String -> Pledge IO (RE Term) () -> IO ()
printResult name prog = do
    (_, preC, postC, futC) <- runPledge prog
    putStrLn $ "=== " ++ name ++ " ==="
    putStrLn $ "Pre:    " ++ show (normalize preC)
    putStrLn $ "Post:   " ++ show (normalize postC)
    putStrLn $ "Future: " ++ show (normalize futC)
    putStrLn ""

main :: IO ()
main = do
    printResult "fullSession"      fullSession
    printResult "stalledHandshake" stalledHandshake
    printResult "outOfOrder"       outOfOrder
    printResult "teardownMissed"   teardownMissed

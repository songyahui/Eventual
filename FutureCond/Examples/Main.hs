{-# OPTIONS_GHC -i.. #-}
import qualified Examples.UnitTest        as UnitTest
import qualified Examples.Memory          as Memory
import qualified Examples.FileHandle      as FileHandle
import qualified Examples.Mutex           as Mutex
import qualified Examples.Transaction     as Transaction
import qualified Examples.CryptoSession   as CryptoSession
import qualified Examples.NetworkProtocol as NetworkProtocol
import qualified Examples.Capability      as Capability
import qualified Examples.Sensor          as Sensor

section :: String -> IO ()
section title = putStrLn $ "\n── " ++ title ++ " " ++ replicate (50 - length title) '─'

main :: IO ()
main = do
    section "0. Unit Tests"
    UnitTest.main

    section "1. Memory Management (malloc/free)"
    Memory.main

    section "2. File Handle Lifecycle"
    FileHandle.main

    section "3. Mutex / Lock Lifecycle"
    Mutex.main

    section "4. Database Transactions"
    Transaction.main

    section "5. Cryptographic Sessions & Nonces"
    CryptoSession.main

    section "6. Network Protocol (TCP-like)"
    NetworkProtocol.main

    section "7. Capability / Token Lifecycle"
    Capability.main

    section "8. Sensor / Actuator (IoT)"
    Sensor.main

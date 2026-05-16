{-# OPTIONS_GHC -i.. #-}
import qualified Examples.UnitTest           as UnitTest
import qualified Examples.RE.Memory          as REMemory
import qualified Examples.RE.FileHandle      as REFileHandle
import qualified Examples.RE.Mutex           as REMutex
import qualified Examples.RE.Transaction     as RETransaction
import qualified Examples.RE.CryptoSession   as RECryptoSession
import qualified Examples.RE.NetworkProtocol as RENetworkProtocol
import qualified Examples.RE.Capability      as RECapability
import qualified Examples.RE.Sensor          as RESensor
import qualified Examples.RE.Shadow          as REShadow
import qualified Examples.SL.HeapMemory      as SLHeapMemory
import qualified Examples.SL.BankAccount     as SLBankAccount
import qualified Examples.SL.LinkedList      as SLLinkedList

section :: String -> IO ()
section title = putStrLn $ "\n── " ++ title ++ " " ++ replicate (50 - length title) '─'

main :: IO ()
main = do
    section "0. Unit Tests"
    UnitTest.main

    section "RE 1. Memory Management (malloc/free)"
    REMemory.main

    section "RE 2. File Handle Lifecycle"
    REFileHandle.main

    section "RE 3. Mutex / Lock Lifecycle"
    REMutex.main

    section "RE 4. Database Transactions"
    RETransaction.main

    section "RE 5. Cryptographic Sessions & Nonces"
    RECryptoSession.main

    section "RE 6. Network Protocol (TCP-like)"
    RENetworkProtocol.main

    section "RE 7. Capability / Token Lifecycle"
    RECapability.main

    section "RE 8. Sensor / Actuator (IoT)"
    RESensor.main

    section "RE 9. Shadow Approach (spec alongside IO)"
    REShadow.main

    section "SL 1. Heap Memory (alloc/free/read/write)"
    SLHeapMemory.main

    section "SL 2. Bank Account (Pure Presburger guards)"
    SLBankAccount.main

    section "SL 3. Linked List (SepStar ownership)"
    SLLinkedList.main

{-# OPTIONS_GHC -i../.. #-}
module Examples.RE.Sensor where
import Prelude hiding ((<>))
import FutureCond

sensorInit :: Int -> FutureCond RE ()
sensorInit sid = FutureCond
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "sensorInit" (List [Num sid]))
    , future = \_ -> finally (Atom "sensorSleep" (List [Num sid]))
    }

sensorRead :: Int -> FutureCond RE ()
sensorRead sid = FutureCond
    { ret    = ()
    , pre    = Or (Single (Atom "sensorInit" (List [Num sid])))
                  (Single (Atom "sensorRead" (List [Num sid])))
    , post   = Single (Atom "sensorRead" (List [Num sid]))
    , future = \_ -> universe
    }

-- Precondition: sensor must have been initialised or read before sleeping
sensorSleep :: Int -> FutureCond RE ()
sensorSleep sid = FutureCond
    { ret    = ()
    , pre    = Or (Single (Atom "sensorInit" (List [Num sid])))
                  (Single (Atom "sensorRead" (List [Num sid])))
    , post   = Single (Atom "sensorSleep" (List [Num sid]))
    , future = \_ -> universe
    }

motorOn :: Int -> FutureCond RE ()
motorOn mid = FutureCond
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "motorOn" (List [Num mid]))
    , future = \_ -> finally (Atom "motorOff" (List [Num mid]))
    }

motorOff :: Int -> FutureCond RE ()
motorOff mid = FutureCond
    { ret    = ()
    , pre    = Single (Atom "motorOn" (List [Num mid]))
    , post   = Single (Atom "motorOff" (List [Num mid]))
    , future = \_ -> universe
    }

actuate :: String -> Int -> FutureCond RE ()
actuate device level = FutureCond
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "actuate" (List [Str device, Num level]))
    , future = \_ -> universe
    }

-- Good: init, read, sleep
safeSensorCycle :: FutureCond RE ()
safeSensorCycle = do
    sensorInit 1
    sensorRead 1
    sensorSleep 1

-- Good: motor on, actuate, motor off
safeMotorCycle :: FutureCond RE ()
safeMotorCycle = do
    motorOn 1
    actuate "pump" 80
    motorOff 1

-- Bad: sensor 2 never slept — future pending
sensorLeftOn :: FutureCond RE ()
sensorLeftOn = do
    sensorInit 1
    sensorSleep 1
    sensorInit 2
    sensorRead 2
    -- sensorSleep 2 missing

-- Bad: motor left running — future pending
motorLeftRunning :: FutureCond RE ()
motorLeftRunning = do
    motorOn 3
    actuate "fan" 50

-- Bad: sensorRead without init — precondition violated
readWithoutInit :: FutureCond RE ()
readWithoutInit = sensorRead 5

printResult :: String -> FutureCond RE () -> IO ()
printResult name prog = do
    putStrLn $ "=== " ++ name ++ " ==="
    putStrLn $ "Pre:    " ++ show (normalize (pre    prog))
    putStrLn $ "Post:   " ++ show (normalize (post   prog))
    putStrLn $ "Future: " ++ show (normalize (evalFuture prog))
    putStrLn ""

main :: IO ()
main = do
    printResult "safeSensorCycle"  safeSensorCycle
    printResult "safeMotorCycle"   safeMotorCycle
    printResult "sensorLeftOn"     sensorLeftOn
    printResult "motorLeftRunning" motorLeftRunning
    printResult "readWithoutInit"  readWithoutInit

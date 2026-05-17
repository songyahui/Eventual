{-# OPTIONS_GHC -i../.. #-}
module Examples.RE.Sensor where
import Prelude hiding ((<>))
import Pledge

sensorInit :: Int -> Pledge RE ()
sensorInit sid = Pledge
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "sensorInit" (List [Num sid]))
    , future = \_ -> finally (Atom "sensorSleep" (List [Num sid]))
    }

sensorRead :: Int -> Pledge RE ()
sensorRead sid = Pledge
    { ret    = ()
    , pre    = Or (Single (Atom "sensorInit" (List [Num sid])))
                  (Single (Atom "sensorRead" (List [Num sid])))
    , post   = Single (Atom "sensorRead" (List [Num sid]))
    , future = const universe
    }

-- Precondition: sensor must have been initialised or read before sleeping
sensorSleep :: Int -> Pledge RE ()
sensorSleep sid = Pledge
    { ret    = ()
    , pre    = Or (Single (Atom "sensorInit" (List [Num sid])))
                  (Single (Atom "sensorRead" (List [Num sid])))
    , post   = Single (Atom "sensorSleep" (List [Num sid]))
    , future = const universe
    }

motorOn :: Int -> Pledge RE ()
motorOn mid = Pledge
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "motorOn" (List [Num mid]))
    , future = \_ -> finally (Atom "motorOff" (List [Num mid]))
    }

motorOff :: Int -> Pledge RE ()
motorOff mid = Pledge
    { ret    = ()
    , pre    = Single (Atom "motorOn" (List [Num mid]))
    , post   = Single (Atom "motorOff" (List [Num mid]))
    , future = const universe
    }

actuate :: String -> Int -> Pledge RE ()
actuate device level = Pledge
    { ret    = ()
    , pre    = universe
    , post   = Single (Atom "actuate" (List [Str device, Num level]))
    , future = const universe
    }

-- Good: init, read, sleep
safeSensorCycle :: Pledge RE ()
safeSensorCycle = do
    sensorInit 1
    sensorRead 1
    sensorSleep 1

-- Good: motor on, actuate, motor off
safeMotorCycle :: Pledge RE ()
safeMotorCycle = do
    motorOn 1
    actuate "pump" 80
    motorOff 1

-- Bad: sensor 2 never slept — future pending
sensorLeftOn :: Pledge RE ()
sensorLeftOn = do
    sensorInit 1
    sensorSleep 1
    sensorInit 2
    sensorRead 2
    -- sensorSleep 2 missing

-- Bad: motor left running — future pending
motorLeftRunning :: Pledge RE ()
motorLeftRunning = do
    motorOn 3
    actuate "fan" 50

-- Bad: sensorRead without init — precondition violated
readWithoutInit :: Pledge RE ()
readWithoutInit = sensorRead 5

printResult :: String -> Pledge RE () -> IO ()
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

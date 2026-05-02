{-# OPTIONS_GHC -i.. #-}
module Examples.Sensor where
import Prelude hiding ((<>))
import Future

-- IoT sensor / actuator lifecycle.
-- Activating a device creates a future obligation to deactivate it.
-- Turning a motor on requires it to be turned off.

-- Initialise a sensor: future = eventually sensorSleep(id)
sensorInit :: Int -> Effectful RE ()
sensorInit sid = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single ("sensorInit", [Num sid])
    , future = finally ("sensorSleep", [Num sid])
    }

-- Read from a sensor: no new obligation
sensorRead :: Int -> Effectful RE ()
sensorRead sid = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single ("sensorRead", [Num sid])
    , future = anything
    }

-- Put sensor to sleep: discharges sensorInit obligation
sensorSleep :: Int -> Effectful RE ()
sensorSleep sid = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single ("sensorSleep", [Num sid])
    , future = anything
    }

-- Turn motor on: future = eventually motorOff(id)
motorOn :: Int -> Effectful RE ()
motorOn mid = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single ("motorOn", [Num mid])
    , future = finally ("motorOff", [Num mid])
    }

-- Turn motor off: discharges motorOn obligation
motorOff :: Int -> Effectful RE ()
motorOff mid = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single ("motorOff", [Num mid])
    , future = anything
    }

-- Actuate output device: no obligation
actuate :: String -> Int -> Effectful RE ()
actuate device level = Effectful
    { ret    = ()
    , pre    = universe
    , post   = Single ("actuate", [Str device, Num level])
    , future = anything
    }

-- Good: sensor init, read, sleep
safeSensorCycle :: Effectful RE ()
safeSensorCycle = do
    sensorInit 1
    sensorRead 1
    sensorRead 1
    sensorSleep 1

-- Good: motor on, work done, motor off
safeMotorCycle :: Effectful RE ()
safeMotorCycle = do
    motorOn 1
    actuate "pump" 80
    motorOff 1

-- Good: multi-device workflow — sensor triggers motor, both cleaned up
coordinatedWorkflow :: Effectful RE ()
coordinatedWorkflow = do
    sensorInit 1
    motorOn 2
    sensorRead 1
    actuate "valve" 100
    motorOff 2
    sensorSleep 1

-- Bad: sensor never put to sleep (power drain, resource leak)
sensorLeftOn :: Effectful RE ()
sensorLeftOn = do
    sensorInit 1
    sensorRead 1
    sensorInit 2
    sensorRead 2
    sensorSleep 1
    -- sensor 2 never slept

-- Bad: motor left running (safety hazard)
motorLeftRunning :: Effectful RE ()
motorLeftRunning = do
    motorOn 3
    actuate "fan" 50

printResult :: String -> Effectful RE () -> IO ()
printResult name prog = do
    putStrLn $ "=== " ++ name ++ " ==="
    putStrLn $ "Post:   " ++ show (post prog)
    putStrLn $ "Future: " ++ show (normalize (future prog))
    putStrLn ""

main :: IO ()
main = do
    printResult "safeSensorCycle"      safeSensorCycle
    printResult "safeMotorCycle"       safeMotorCycle
    printResult "coordinatedWorkflow"  coordinatedWorkflow
    printResult "sensorLeftOn"         sensorLeftOn
    printResult "motorLeftRunning"     motorLeftRunning

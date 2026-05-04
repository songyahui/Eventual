{-# OPTIONS_GHC -i.. #-}
module Examples.UnitTest where
import Prelude hiding ((<>))
import Future

test_derivative :: IO ()
test_derivative = do
    let r    = finally (Atom "free" [Num 1])
        e    = Atom "free" [Num 1]
        deri = normalize (derivative e r)
    if deri == universe
        then putStrLn "Derivative test passed!"
        else putStrLn $ "Derivative test FAILED: " ++ show deri

main :: IO ()
main = do
    test_derivative
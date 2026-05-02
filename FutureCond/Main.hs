import Prelude hiding ((<>))
import qualified Control.Exception as Control (assert)
import Future

malloc :: Int -> Effectful RE ()
malloc addr = Effectful
    { ret = ()
    , pre = universe
    , post = Single ("malloc", [Num addr])
    , future = finally ("free", [Num addr])
    }

free :: Int -> Effectful RE ()
free addr = Effectful
    { ret = ()
    , pre = universe
    , post = Single ("free", [Num addr])
    , future = anything
    }

test1 :: Effectful RE ()
test1 = do
    malloc 1
    malloc 2
    free 1

test_derivitive :: IO ()
test_derivitive = do
    let r = finally ("free", [Num 1])
        e = Pos ("free", [Num 1])
        deri = normalize (derivitive e r)
    Control.assert (deri == anything) (putStrLn "Derivitive test 1 passed!")

main :: IO ()
main = do
    test_derivitive
    let result = test1
    putStrLn $ "Result: " ++ show (ret result)
    putStrLn $ "Pre Condition: " ++ show (pre result)
    putStrLn $ "Post Condition: " ++ show (post result)
    putStrLn $ "Future Condition: " ++ show (normalize (future result))

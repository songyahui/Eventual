type Trace = String
type FutureCond = [String]

data Effectful a = Effectful
    { ret :: a
    , trace :: Trace
    , futureCondition :: FutureCond
    }

malloc :: Effectful ()
malloc = Effectful
    { ret = ()
    , trace = "malloc"
    , futureCondition = ["free"]
    }

free :: Effectful ()
free = Effectful
    { ret = ()
    , trace = "free"
    , futureCondition = []
    }

instance Functor Effectful where
    fmap f e = Effectful
        { ret = f (ret e)
        , trace = trace e
        , futureCondition = futureCondition e
        }

subtraction :: Trace -> FutureCond -> FutureCond
subtraction t [] = []
subtraction t (fc:rest) =
    if fc == t then rest else fc : subtraction t rest

instance Applicative Effectful where
    pure x = Effectful
        { ret = x
        , trace = ""
        , futureCondition = []
        }
    ef <*> ex = Effectful
        { ret = (ret ef) (ret ex)
        , trace = trace ef ++ trace ex
        , futureCondition = (subtraction (trace ex) (futureCondition ef)) ++ futureCondition ex
        }

instance Monad Effectful where
    return = pure
    e >>= f = Effectful
        { ret = ret (f (ret e))
        , trace = trace e ++ trace (f (ret e))
        , futureCondition = (subtraction (trace (f (ret e))) (futureCondition e)) ++ futureCondition (f (ret e))
        }

test1 :: Effectful ()
test1 = do
    malloc
    malloc
    free

main :: IO ()
main = do
    let result = test1
    putStrLn $ "Result: " ++ show (ret result)
    putStrLn $ "Trace: " ++ trace result
    putStrLn $ "Future Condition: " ++ show (futureCondition result)
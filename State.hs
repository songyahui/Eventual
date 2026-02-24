newtype State s a = State { runState :: s -> (a, s) }

-- Functor
instance Functor (State s) where
    fmap f (State g) = State $ \s ->
        let (a, s') = g s
        in (f a, s')

-- Applicative
instance Applicative (State s) where
    pure a = State $ \s -> (a, s)
    (State f) <*> (State g) = State $ \s ->
        let (func, s')  = f s
            (val,  s'') = g s'
        in (func val, s'')

-- Monad
instance Monad (State s) where
    return = pure
    (State g) >>= f = State $ \s ->
        let (a, s')     = g s
            (State h)   = f a
        in h s'

-- Get the current state
get :: State s s
get = State $ \s -> (s, s)

-- Replace the state
put :: s -> State s ()
put s = State $ \_ -> ((), s)

-- Modify the state with a function
modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

-- Run and get both result and final state
run :: State s a -> s -> (a, s)
run (State f) s = f s

-- Run and get only the result
evalState :: State s a -> s -> a
evalState m s = fst (run m s)

-- Run and get only the final state
execState :: State s a -> s -> s
execState m s = snd (run m s)

type Counter = Int

increment :: State Counter ()
increment = modify (+1)

decrement :: State Counter ()
decrement = modify (subtract 1)

reset :: State Counter ()
reset = put 0

getCount :: State Counter Int
getCount = get

-- Run it
counterApp :: State Counter Int
counterApp = do
    increment
    increment
    increment
    decrement
    increment
    getCount

main :: IO ()
main = do
    let (result, finalState) = runState counterApp 0
    putStrLn $ "Count: " ++ show result        -- 3
    putStrLn $ "Final State: " ++ show finalState  -- 3
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

--------

data Term = 
      Unit
    | Var String
    | Str String
    | Num Int
    | Pointer Int
    | Null
    deriving (Show)

type Event = (String, [Term])

data RE = Bot
        | Empty
        | Single Event
        | Neg [Term]
        | Wildcard
        | Seq RE RE
        | Or RE RE
        | And RE RE
        | Star RE
        deriving (Show)

type Trace = RE
type FutureCond = RE
type ProgramState = (Trace, FutureCond)

defaultFC :: FutureCond
defaultFC = Star Wildcard

finally :: FutureCond -> FutureCond
finally fc = Seq defaultFC (Seq fc defaultFC)

globally :: FutureCond -> FutureCond
globally fc = Star fc

ev :: Event -> State ProgramState Term
ev arg = State $ \(trace, fc) ->
    let newTrace = Seq trace (Single arg)
    in (Unit, (newTrace, fc))

assertFC :: FutureCond -> State ProgramState Term
assertFC cond = State $ \(trace, fc) ->
    let newFC = And fc cond
    in (Unit, (trace, newFC))

class Effectful a where
    ret :: a -> Term
    trace :: a -> Trace
    futureCondition :: a -> FutureCond

traceSubtraction :: FutureCond -> Trace -> FutureCond
traceSubtraction fc tr = fc

stateConcat :: (Effectful a) => a -> State ProgramState Term
stateConcat a = State $ \(t, fc) ->
    let newTrace = Seq t (trace a)
        newFC = And fc (traceSubtraction (futureCondition a) (trace a))
    in (ret a, (newTrace, newFC))

programStateApp :: State ProgramState Term
programStateApp = do
    ev ("malloc", [Num 4])
    ev ("free", [Num 4])
    assertFC (Single ("free", [Num 4]))
    return Unit

main1 :: IO ()
main1 = do
    let initialState = (Empty, defaultFC)
    let (result, finalState) = runState programStateApp initialState
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Final State: " ++ show finalState
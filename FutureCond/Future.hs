{-# LANGUAGE FunctionalDependencies #-}

import qualified Debug.Trace as Debug
import qualified Control.Exception as Control (assert)

data Term =
      Var String
    | Str String
    | Num Int
    deriving (Eq)

instance Show Term where
    show (Var s) = s
    show (Str s) = "\"" ++ s ++ "\""
    show (Num n) = show n

type Event = (String, [Term])

string_of_event :: Event -> String
string_of_event (name, args) = name ++ "(" ++ concat (map show args) ++ ")"

data Fst = Pos Event | Neg Event | W deriving (Eq, Show)

data RE = Bot
        | Epsilon
        | Single Event
        | Wildcard
        | Seq RE RE
        | Or RE RE
        | And RE RE
        | Star RE
        deriving (Eq)

instance Show RE where
    show Bot = "∅"
    show Epsilon = "ε"
    show (Single e) = string_of_event e
    show Wildcard = "_"
    show (Seq r1 r2) = "(" ++ show r1 ++ ") . (" ++ show r2 ++ ")"
    show (Or r1 r2) = "(" ++ show r1 ++ ") \\/ (" ++ show r2 ++ ")"
    show (And r1 r2) = "(" ++ show r1 ++ ") /\\ (" ++ show r2 ++ ")"
    show (Star r) = "(" ++ show r ++ ")^*"

type Trace = RE
type FutureCond = RE

class Composable a where
    concatenation :: a -> a -> a
    conjunction :: a -> a -> a
    empty :: a
    universe :: a
    subtraction :: a -> a -> a
    normalize :: a -> a

nullable :: RE -> Bool
nullable r = case r of
    Epsilon -> True
    Star _ -> True
    Seq r1 r2 -> nullable r1 && nullable r2
    Or r1 r2 -> nullable r1 || nullable r2
    And r1 r2 -> nullable r1 && nullable r2
    Wildcard -> False
    Bot -> False
    Single _ -> False

first :: RE -> [Fst]
first r = case r of
    Bot -> []
    Epsilon -> []
    Single e -> [Pos e]
    Wildcard -> [W]
    Seq r1 r2 -> if nullable r1 then first r1 ++ first r2 else first r1
    Or r1 r2 -> first r1 ++ first r2
    And r1 r2 ->
        let intersect xs ys = [x | x <- xs, x `elem` ys] in
        intersect (first r1) (first r2)
    Star r -> first r

derivitive :: Fst -> RE -> RE
derivitive e r = case r of
    Bot -> Bot
    Epsilon -> Bot
    Single e' -> case e of
        Pos e'' -> if e'' == e' then Epsilon else Bot
        Neg e'' -> if e'' /= e' then Epsilon else Bot
        W -> Bot
    Wildcard -> Epsilon
    Seq r1 r2 -> if nullable r1 then Or (Seq (derivitive e r1) r2) (derivitive e r2) else Seq (derivitive e r1) r2
    Or r1 r2 -> Or (derivitive e r1) (derivitive e r2)
    And r1 r2 -> And (derivitive e r1) (derivitive e r2)
    Star r -> Seq (derivitive e r) (Star r)

instance Composable RE where
    concatenation r1 r2 = Seq r1 r2

    conjunction r1 r2 = And r1 r2

    empty = Epsilon
    universe = anything

    subtraction Epsilon r2 = r2
    subtraction r1 r2 =
        let fst = first r1
            res = map (\e ->
                    let r1' = derivitive e r1
                        r2' = derivitive e r2
                    in subtraction (normalize r1') (normalize r2')
                    ) fst
        in foldr Or Bot res

    normalize r = case r of
        Seq r1 r2 -> 
            case (normalize r1, normalize r2) of
                (Bot, _) -> Bot
                (_, Bot) -> Bot
                (Epsilon, r2') -> normalize r2'
                (r1', Epsilon) -> normalize r1'
                (r1', r2') -> Seq r1' r2'

        Or r1 r2 -> 
            case (normalize r1, normalize r2) of
                (Bot, r2') -> normalize r2'
                (r1', Bot) -> normalize r1'
                (r1', Star Wildcard) -> Star Wildcard
                (Star Wildcard, r2') -> Star Wildcard
                (r1', r2') -> Or r1' r2'

        And r1 r2 -> 
            case (normalize r1, normalize r2) of
                (Bot, _) -> Bot
                (_, Bot) -> Bot
                (Epsilon, r2') -> if nullable r2' then r2 else Bot
                (r1', Epsilon) -> if nullable r1' then r1 else Bot
                (r1', Star Wildcard) -> r1'
                (Star Wildcard, r2') -> r2'
                (r1', r2') -> if r1' == r2' then r1' else And r1' r2'

        Star Bot -> Epsilon
        Star Epsilon -> Bot
        _ -> r

anything :: FutureCond
anything = Star Wildcard

finally :: Event -> FutureCond
finally e = Seq anything (Seq (Single e) anything)

data Effectful eff a = Effectful
    { ret :: a
    , trace :: eff
    , futureCondition :: eff
    }

instance Functor (Effectful eff) where
    fmap f e = Effectful
        { ret = f (ret e)
        , trace = trace e
        , futureCondition = futureCondition e
        }

instance (Composable eff) => Applicative (Effectful eff) where
    pure x = Effectful
        { ret = x
        , trace = empty
        , futureCondition = universe
        }
    ef <*> ex = Effectful
        { ret = (ret ef) (ret ex)
        , trace = concatenation (trace ef) (trace ex)
        , futureCondition = conjunction (subtraction (trace ex) (futureCondition ef)) (futureCondition ex)
        }


instance (Composable eff) => Monad (Effectful eff) where
    return = pure
    e >>= f = Effectful
        { ret = ret (f (ret e))
        , trace = concatenation (trace e) (trace (f (ret e)))
        , futureCondition = conjunction (subtraction (trace (f (ret e))) (futureCondition e)) (futureCondition (f (ret e)))
        }

malloc :: Int -> Effectful RE ()
malloc addr = Effectful
    { ret = ()
    , trace = Single ("malloc", [Num addr])
    , futureCondition = finally ("free", [Num addr])
    }

free :: Int -> Effectful RE ()
free addr = Effectful
    { ret = ()
    , trace = Single ("free", [Num addr])
    , futureCondition = anything
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
    putStrLn $ "Trace: " ++ show (trace result)
    putStrLn $ "Future Condition: " ++ show (normalize (futureCondition result))

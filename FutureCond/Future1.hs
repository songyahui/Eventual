{-# LANGUAGE FunctionalDependencies #-}
data Term =
      Var String
    | Str String
    | Num Int
    deriving (Eq, Show)

type Event = (String, [Term])

data Fst = Pos Event | Neg Event | W deriving (Eq, Show)

data RE = Bot
        | Epsilon
        | Single Event
        | Wildcard
        | Seq RE RE
        | Or RE RE
        | And RE RE
        | Star RE
        deriving (Eq, Show)

type Trace = RE
type FutureCond = RE

class Composable a b | a -> b where
    concate :: a -> a -> a
    conjunction :: a -> a -> a
    empty :: a
    universe :: a
    nullable :: a -> Bool
    first :: a -> [b]
    derivitive :: b -> a -> a
    subtraction :: a -> a -> a
    simplify :: a -> a

instance Composable RE Fst where
    concate r1 r2 = Seq r1 r2

    conjunction r1 r2 = And r1 r2

    empty = Epsilon
    universe = Star Wildcard

    nullable r = case r of
        Epsilon -> True
        Star _ -> True
        Seq r1 r2 -> nullable r1 && nullable r2
        Or r1 r2 -> nullable r1 || nullable r2
        And r1 r2 -> nullable r1 && nullable r2
        _ -> False

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
    
    subtraction Epsilon r2 = r2
    subtraction Bot r2 = Bot
    subtraction r1 r2 = 
        let fst = first r1
            res = map (\e ->
                        let r1' = derivitive e r1
                            r2' = derivitive e r2
                        in subtraction (simplify r1') (simplify r2')
                        ) fst
        in foldr Or Bot res

    simplify r = case r of
        Seq Bot _ -> Bot
        Seq _ Bot -> Bot
        Seq Epsilon r2 -> simplify r2
        Seq r1 Epsilon -> simplify r1
        Or Bot r2 -> simplify r2
        Or r1 Bot -> simplify r1
        And Bot _ -> Bot
        And _ Bot -> Bot
        And Epsilon r2 -> if nullable r2 then r2 else Bot
        And r1 Epsilon -> if nullable r1 then r1 else Bot
        Star Bot -> Epsilon
        Star Epsilon -> Bot
        _ -> r


anything :: FutureCond
anything = Star Wildcard

finally :: Event -> FutureCond
finally e = Seq anything (Single e)

data Effectful eff fst a = Effectful
    { ret :: a
    , trace :: eff
    , futureCondition :: eff
    }

malloc :: Effectful RE Fst ()
malloc = Effectful
    { ret = ()
    , trace = Single ("malloc", [])
    , futureCondition = finally ("free", [])
    }

free :: Effectful RE Fst ()
free = Effectful
    { ret = ()
    , trace = Single ("free", [])
    , futureCondition = anything
    }


instance Functor (Effectful eff fst) where
    fmap f e = Effectful
        { ret = f (ret e)
        , trace = trace e
        , futureCondition = futureCondition e
        }

instance (Composable eff fst) => Applicative (Effectful eff fst) where
    pure x = Effectful
        { ret = x
        , trace = empty
        , futureCondition = universe
        }
    ef <*> ex = Effectful
        { ret = (ret ef) (ret ex)
        , trace = concate (trace ef) (trace ex)
        , futureCondition = conjunction (subtraction (trace ex) (futureCondition ef)) (futureCondition ex)
        }


instance (Composable eff fst) => Monad (Effectful eff fst) where
    return = pure
    e >>= f = Effectful
        { ret = ret (f (ret e))
        , trace = concate (trace e) (trace (f (ret e)))
        , futureCondition = conjunction (subtraction (trace (f (ret e))) (futureCondition e)) (futureCondition (f (ret e)))
        }

test1 :: Effectful RE Fst ()
test1 = do
    malloc
    malloc
    free

main :: IO ()
main = do
    let result = test1
    putStrLn $ "Result: " ++ show (ret result)
    putStrLn $ "Trace: " ++ show (trace result)
    putStrLn $ "Future Condition: " ++ show (futureCondition result)

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

type Trace = RE
type FutureCond = RE

defaultFC :: FutureCond
defaultFC = Star Wildcard

finally :: FutureCond -> FutureCond
finally fc = Seq defaultFC (Seq fc defaultFC)

globally :: FutureCond -> FutureCond
globally fc = Star fc

class Effectful a where
    ret :: a -> Term
    trace :: a -> Trace
    futureCondition :: a -> FutureCond

type Address = Int
data Program = Val Term
             | MallocExpr Address Int 
             | FreeExpr Address
             | LetExpr String Program Program
          deriving (Show)

substituteTerm :: String -> Term -> Term -> Term
substituteTerm var term t =
    case t of
        Unit -> Unit
        Str s -> Str s
        Num n -> Num n
        Pointer p -> Pointer p
        Null -> Null
        Var v -> if v == var then term else Var v

substitute :: String -> Term -> Program -> Program
substitute var term p = 
    case p of
        Val t -> Val (substituteTerm var term t)
        MallocExpr address size -> MallocExpr address size
        FreeExpr address -> FreeExpr address
        LetExpr v p1 p2 -> 
            let p1' = substitute var term p1
                p2' = if v == var then p2 else substitute var term p2 in
            LetExpr v p1' p2'

instance Effectful Program where
    ret p = case p of
        Val e -> e
        MallocExpr address size -> Pointer address
        FreeExpr address -> Unit
        LetExpr var p1 p2 -> 
            let ret' = ret p1
                p2' = substitute var ret' p2 in
            ret p2'

    trace p = case p of
        Val e -> Single ("ret", [e])
        MallocExpr address size -> Single ("malloc", [Pointer address])
        FreeExpr address -> Single ("free", [Pointer address])
        LetExpr var p1 p2 -> Seq (trace p1) (trace p2)

    futureCondition p = case p of
        Val _ -> defaultFC
        MallocExpr address _ -> finally (Single ("free", [Pointer address]))
        FreeExpr address -> globally (Neg [Pointer address])
        LetExpr var p1 p2 -> And (futureCondition p1) (futureCondition p2)

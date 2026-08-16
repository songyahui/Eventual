module Examples.RE.Simple where
import Prelude hiding ((<>))
import Pledge
import qualified System.IO as IO
import System.Random

plus :: Int -> Int -> Pledge RE Int
plus a b =
    let res = a + b in
    Pledge
    { ret    = res
    , pre    = universe
    , post   = Single (Atom "plus" (List [Num a, Num b]))
    , future = finally (Atom "div" (List [Num a, Num b]))
    }

div' :: Int -> Int -> Pledge RE Int
div' a b =
    let res = a `Prelude.div` b in
    Pledge
    { ret    = res
    , pre    = previously (Atom "plus" (List [Num a, Num b]))
    , post   = Single (Atom "div" (List [Num a, Num b]))
    , future = universe
    }

main :: IO ()
main = do
    printOfPledgeRE "PLUS" (plus 1 2)
    printOfPledgeRE "DIV" (div' 1 2)
    printOfPledgeRE "PLUS + DIV" (plus 1 2 >> div' 1 2)
    return ()

{-# OPTIONS_GHC -i../.. #-}
module Examples.RE.Simple where
import Prelude hiding ((<>))
import Pledge
import qualified System.IO as IO
import System.Random

plus :: Int -> Int -> Pledge RE Int
plus a b =
    let res = a + b in
    Pledge
    { ret    = return res
    , pre    = return universe
    , post   = return $ Single (Atom "plus" (List [Num a, Num b]))
    , future = return $ finally (Atom "div" (List [Num a, Num b]))
    }

div' :: Int -> Int -> Pledge RE Int
div' a b =
    let res = a `Prelude.div` b in
    Pledge
    { ret    = return res
    , pre    = return $ previously (Atom "plus" (List [Num a, Num b]))
    , post   = return $ Single (Atom "div" (List [Num a, Num b]))
    , future = return universe
    }

main :: IO ()
main = do
    printOfPledgeRE "PLUS" (plus 1 2)
    printOfPledgeRE "DIV" (div' 1 2)
    printOfPledgeRE "PLUS + DIV" (plus 1 2 >> div' 1 2)
    return ()

module Main where

import qualified Test.HUnit as HUnit
import Control.Monad (unless)
import qualified Data.Set as DataSet
import HedgeDeriv

main :: IO ()
main = do { 
    counts <- HUnit.runTestTT $ HUnit.TestLabel "reachable" $ HUnit.TestCase $ 
        let p = Capture (Concat (Capture (NodePattern "a" (Name "n1")) (Var "y")) (NodePattern "c" (Name "n3"))) (Var "x")
            n1 = Capture (Or (NodePattern "b" (Name "n2")) EmptyPattern) (Var "z")
            n2 = Capture EmptyPattern (Var "w")
            n3 = EmptyPattern
            refs = newRef (Name "n1") n1 `union` newRef (Name "n2") n2 `union` newRef (Name "n3") n3
            got = reachable refs p
            want = DataSet.fromList [(Var "w"), (Var "x"), (Var "y"), (Var "z")]
        in unless (got == want) $ HUnit.assertFailure $ "want: " ++ show want ++ " got: " ++ show got;
    putStrLn $ show counts;
    
    return ()
}
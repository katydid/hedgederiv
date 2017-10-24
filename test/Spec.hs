module Main where

import qualified Test.HUnit as HUnit
import Control.Monad (unless)
import qualified Data.Set as DataSet
import HedgeDeriv

first :: (Refs String, Pattern String)
first =
    let p = Capture (Concat (Capture (NodePattern "a" (Name "n1")) (Var "y")) (NodePattern "c" (Name "n3"))) (Var "x")
        n1 = Capture (Or (NodePattern "b" (Name "n2")) EmptyPattern) (Var "z")
        n2 = Capture EmptyPattern (Var "w")
        n3 = EmptyPattern
        refs = newRef (Name "n1") n1 `union` newRef (Name "n2") n2 `union` newRef (Name "n3") n3
    in (refs, p)

changen1 :: (Refs String, Pattern String)
changen1 =
    let n1 = Capture (Or (NodePattern "b" (Name "n1")) EmptyPattern) (Var "z")
        refs = newRef (Name "n1") n1
    in (refs, n1)

main :: IO ()
main = do { 
    HUnit.runTestTT $ HUnit.TestLabel "reachable" $ HUnit.TestCase $ 
        let (refs, p) = first
            got = reachable refs p
            want = DataSet.fromList [(Var "w"), (Var "x"), (Var "y"), (Var "z")]
        in unless (got == want) $ HUnit.assertFailure $ "want: " ++ show want ++ " got: " ++ show got;

    HUnit.runTestTT $ HUnit.TestLabel "linear success" $ HUnit.TestCase $ 
        let (refs, p) = first
            got = isLinear refs p
            want = True
        in unless (got == want) $ HUnit.assertFailure $ "want: " ++ show want ++ " got: " ++ show got;

    HUnit.runTestTT $ HUnit.TestLabel "linear failure" $ HUnit.TestCase $ 
        let (refs, p) = changen1
            got = isLinear refs p
            want = False
        in unless (got == want) $ HUnit.assertFailure $ "want: " ++ show want ++ " got: " ++ show got;
    
    return ()
}
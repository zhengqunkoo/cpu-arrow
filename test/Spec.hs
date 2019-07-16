import CpuArrow.Base
import CpuArrow.Logic
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [logicTests]

logicTests :: TestTree
logicTests = testGroup "Logic" [andTests, orTests, notTests, xorTests]

andTests :: TestTree
andTests =
  testCase "And" $ do
    runCircuit aAnd (False, False) @=? False
    runCircuit aAnd (False, True) @=? False
    runCircuit aAnd (True, False) @=? False
    runCircuit aAnd (True, True) @=? True

orTests :: TestTree
orTests =
  testCase "Or" $ do
    runCircuit aOr (False, False) @=? False
    runCircuit aOr (False, True) @=? True
    runCircuit aOr (True, False) @=? True
    runCircuit aOr (True, True) @=? True

notTests :: TestTree
notTests =
  testCase "Not" $ do
    runCircuit aNot False @=? True
    runCircuit aNot True @=? False

xorTests :: TestTree
xorTests =
  testCase "Xor" $ do
    runCircuit aXor (False, False) @=? False
    runCircuit aXor (False, True) @=? True
    runCircuit aXor (True, False) @=? True
    runCircuit aXor (True, True) @=? False

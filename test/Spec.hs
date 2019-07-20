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
    runCircuit aAnd [(False, False), (False, True), (True, False), (True, True)] @=?
      [False, False, False, True]

orTests :: TestTree
orTests =
  testCase "Or" $ do
    runCircuit aOr [(False, False), (False, True), (True, False), (True, True)] @=?
      [False, True, True, True]

notTests :: TestTree
notTests = testCase "Not" $ do runCircuit aNot [False, True] @=? [True, False]

xorTests :: TestTree
xorTests =
  testCase "Xor" $ do
    runCircuit aXor [(False, False), (False, True), (True, False), (True, True)] @=?
      [False, True, True, False]

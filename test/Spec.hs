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
    runCircuit aAnd ([(0, 0), (0, 1), (1, 0), (1, 1)] :: [(Int, Int)]) @=?
      [0, 0, 0, 1]

orTests :: TestTree
orTests =
  testCase "Or" $ do
    runCircuit aOr ([(0, 0), (0, 1), (1, 0), (1, 1)] :: [(Int, Int)]) @=?
      [0, 1, 1, 1]

notTests :: TestTree
notTests = testCase "Not" $ do runCircuit aNot ([0, 1] :: [Int]) @=? [1, 0]

xorTests :: TestTree
xorTests =
  testCase "Xor" $ do
    runCircuit aXor ([(0, 0), (0, 1), (1, 0), (1, 1)] :: [(Int, Int)]) @=?
      [0, 1, 1, 0]

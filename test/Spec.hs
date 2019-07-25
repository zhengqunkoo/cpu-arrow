import CpuArrow.Advanced.HalfAdder
import CpuArrow.Advanced.Mux
import CpuArrow.Base
import CpuArrow.Logic
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [logicTests, advancedTests]

logicTests :: TestTree
logicTests = testGroup "Logic" [andTests, orTests, notTests, xorTests]

advancedTests :: TestTree
advancedTests = testGroup "Advanced" [muxesTests, halfAdderTests]

muxesTests :: TestTree
muxesTests = testGroup "Muxes" [muxTests, mux2Tests]

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

muxTests :: TestTree
muxTests =
  testCase "Mux" $ do
    runCircuit
      aMux
      ([ ((0, 0), 0)
       , ((0, 0), 1)
       , ((0, 1), 0)
       , ((0, 1), 1)
       , ((1, 0), 0)
       , ((1, 0), 1)
       , ((1, 1), 0)
       , ((1, 1), 1)
       ] :: [((Int, Int), Int)]) @=?
      [0, 0, 0, 1, 1, 0, 1, 1]

mux2Tests :: TestTree
mux2Tests =
  testCase "Mux2" $ do
    runCircuit
      aMux2
      ([ ((0, 0, 0, 0), (0, 0))
       , ((0, 0, 0, 0), (0, 1))
       , ((0, 0, 0, 0), (1, 0))
       , ((0, 0, 0, 0), (1, 1))
       , ((0, 0, 0, 1), (0, 0))
       , ((0, 0, 0, 1), (0, 1))
       , ((0, 0, 0, 1), (1, 0))
       , ((0, 0, 0, 1), (1, 1))
       , ((0, 0, 1, 0), (0, 0))
       , ((0, 0, 1, 0), (0, 1))
       , ((0, 0, 1, 0), (1, 0))
       , ((0, 0, 1, 0), (1, 1))
       , ((0, 0, 1, 1), (0, 0))
       , ((0, 0, 1, 1), (0, 1))
       , ((0, 0, 1, 1), (1, 0))
       , ((0, 0, 1, 1), (1, 1))
       , ((0, 1, 0, 0), (0, 0))
       , ((0, 1, 0, 0), (0, 1))
       , ((0, 1, 0, 0), (1, 0))
       , ((0, 1, 0, 0), (1, 1))
       , ((0, 1, 0, 1), (0, 0))
       , ((0, 1, 0, 1), (0, 1))
       , ((0, 1, 0, 1), (1, 0))
       , ((0, 1, 0, 1), (1, 1))
       , ((0, 1, 1, 0), (0, 0))
       , ((0, 1, 1, 0), (0, 1))
       , ((0, 1, 1, 0), (1, 0))
       , ((0, 1, 1, 0), (1, 1))
       , ((0, 1, 1, 1), (0, 0))
       , ((0, 1, 1, 1), (0, 1))
       , ((0, 1, 1, 1), (1, 0))
       , ((0, 1, 1, 1), (1, 1))
       , ((1, 0, 0, 0), (0, 0))
       , ((1, 0, 0, 0), (0, 1))
       , ((1, 0, 0, 0), (1, 0))
       , ((1, 0, 0, 0), (1, 1))
       , ((1, 0, 0, 1), (0, 0))
       , ((1, 0, 0, 1), (0, 1))
       , ((1, 0, 0, 1), (1, 0))
       , ((1, 0, 0, 1), (1, 1))
       , ((1, 0, 1, 0), (0, 0))
       , ((1, 0, 1, 0), (0, 1))
       , ((1, 0, 1, 0), (1, 0))
       , ((1, 0, 1, 0), (1, 1))
       , ((1, 0, 1, 1), (0, 0))
       , ((1, 0, 1, 1), (0, 1))
       , ((1, 0, 1, 1), (1, 0))
       , ((1, 0, 1, 1), (1, 1))
       , ((1, 1, 0, 0), (0, 0))
       , ((1, 1, 0, 0), (0, 1))
       , ((1, 1, 0, 0), (1, 0))
       , ((1, 1, 0, 0), (1, 1))
       , ((1, 1, 0, 1), (0, 0))
       , ((1, 1, 0, 1), (0, 1))
       , ((1, 1, 0, 1), (1, 0))
       , ((1, 1, 0, 1), (1, 1))
       , ((1, 1, 1, 0), (0, 0))
       , ((1, 1, 1, 0), (0, 1))
       , ((1, 1, 1, 0), (1, 0))
       , ((1, 1, 1, 0), (1, 1))
       , ((1, 1, 1, 1), (0, 0))
       , ((1, 1, 1, 1), (0, 1))
       , ((1, 1, 1, 1), (1, 0))
       , ((1, 1, 1, 1), (1, 1))
       ] :: [((Int, Int, Int, Int), (Int, Int))]) @=?
      [ 0
      , 0
      , 0
      , 0
      , 0
      , 0
      , 0
      , 1
      , 0
      , 0
      , 1
      , 0
      , 0
      , 0
      , 1
      , 1
      , 0
      , 1
      , 0
      , 0
      , 0
      , 1
      , 0
      , 1
      , 0
      , 1
      , 1
      , 0
      , 0
      , 1
      , 1
      , 1
      , 1
      , 0
      , 0
      , 0
      , 1
      , 0
      , 0
      , 1
      , 1
      , 0
      , 1
      , 0
      , 1
      , 0
      , 1
      , 1
      , 1
      , 1
      , 0
      , 0
      , 1
      , 1
      , 0
      , 1
      , 1
      , 1
      , 1
      , 0
      , 1
      , 1
      , 1
      , 1
      ]

halfAdderTests :: TestTree
halfAdderTests =
  testCase "HalfAdder" $ do
    runCircuit aHalfAdder ([(0, 0), (0, 1), (1, 0), (1, 1)] :: [(Int, Int)]) @=?
      [(0, 0), (0, 1), (0, 1), (1, 0)]

import CpuArrow.Advanced.Demux
import CpuArrow.Advanced.FullAdder
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
advancedTests =
  testGroup
    "Advanced"
    [muxesTests, halfAdderTests, fullAdderTests, demuxesTests]

muxesTests :: TestTree
muxesTests = testGroup "Muxes" [muxTests, mux2Tests]

demuxesTests :: TestTree
demuxesTests = testGroup "Demuxes" [demuxTests, demux2Tests, demux3Tests]

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

fullAdderTests :: TestTree
fullAdderTests =
  testCase "FullAdder" $ do
    runCircuit
      aFullAdder
      ([ ((0, 0), 0)
       , ((0, 0), 1)
       , ((0, 1), 0)
       , ((0, 1), 1)
       , ((1, 0), 0)
       , ((1, 0), 1)
       , ((1, 1), 0)
       , ((1, 1), 1)
       ] :: [((Int, Int), Int)]) @=?
      [(0, 0), (0, 1), (0, 1), (1, 0), (0, 1), (1, 0), (1, 0), (1, 1)]

demuxTests :: TestTree
demuxTests =
  testCase "Demux" $ do
    runCircuit aDemux ([(0, 0), (0, 1), (1, 0), (1, 1)] :: [(Int, Int)]) @=?
      [(0, 0), (0, 0), (0, 1), (1, 0)]

demux2Tests :: TestTree
demux2Tests =
  testCase "Demux2" $ do
    runCircuit
      aDemux2
      ([ (0, (0, 0))
       , (0, (0, 1))
       , (0, (1, 0))
       , (0, (1, 1))
       , (1, (0, 0))
       , (1, (0, 1))
       , (1, (1, 0))
       , (1, (1, 1))
       ] :: [(Int, (Int, Int))]) @=?
      [ (0, 0, 0, 0)
      , (0, 0, 0, 0)
      , (0, 0, 0, 0)
      , (0, 0, 0, 0)
      , (0, 0, 0, 1)
      , (0, 0, 1, 0)
      , (0, 1, 0, 0)
      , (1, 0, 0, 0)
      ]

demux3Tests :: TestTree
demux3Tests =
  testCase "Demux3" $ do
    runCircuit
      aDemux3
      ([ (0, (0, 0, 0))
       , (0, (0, 0, 1))
       , (0, (0, 1, 0))
       , (0, (0, 1, 1))
       , (0, (1, 0, 0))
       , (0, (1, 0, 1))
       , (0, (1, 1, 0))
       , (0, (1, 1, 1))
       , (1, (0, 0, 0))
       , (1, (0, 0, 1))
       , (1, (0, 1, 0))
       , (1, (0, 1, 1))
       , (1, (1, 0, 0))
       , (1, (1, 0, 1))
       , (1, (1, 1, 0))
       , (1, (1, 1, 1))
       ] :: [(Int, (Int, Int, Int))]) @=?
      [ (0, 0, 0, 0, 0, 0, 0, 0)
      , (0, 0, 0, 0, 0, 0, 0, 0)
      , (0, 0, 0, 0, 0, 0, 0, 0)
      , (0, 0, 0, 0, 0, 0, 0, 0)
      , (0, 0, 0, 0, 0, 0, 0, 0)
      , (0, 0, 0, 0, 0, 0, 0, 0)
      , (0, 0, 0, 0, 0, 0, 0, 0)
      , (0, 0, 0, 0, 0, 0, 0, 0)
      , (0, 0, 0, 0, 0, 0, 0, 1)
      , (0, 0, 0, 0, 0, 0, 1, 0)
      , (0, 0, 0, 0, 0, 1, 0, 0)
      , (0, 0, 0, 0, 1, 0, 0, 0)
      , (0, 0, 0, 1, 0, 0, 0, 0)
      , (0, 0, 1, 0, 0, 0, 0, 0)
      , (0, 1, 0, 0, 0, 0, 0, 0)
      , (1, 0, 0, 0, 0, 0, 0, 0)
      ]

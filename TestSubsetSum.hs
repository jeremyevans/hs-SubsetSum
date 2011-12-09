#!/usr/bin/env runhaskell

import Test.HUnit
import SubsetSum

makeTest ass i is = TestLabel "" (TestCase $ ass i is)

assertNoMatch i is = do
  assertBool ("Expected no match but matched: " ++ (show i) ++ " " ++ (show is)) ((subsetSumMap i is) == [])
  r <- subsetSumHash i is
  assertBool ("Expected no match but matched: " ++ (show i) ++ " " ++ (show is)) (r == [])

assertMatch i is = do
  assertBool ("Expected match but no match: " ++ (show i) ++ " " ++ (show is)) ((foldr (+) 0 (subsetSumMap i is)) == i)
  r <- subsetSumHash i is
  assertBool ("Expected match but no match: " ++ (show i) ++ " " ++ (show is)) ((foldr (+) 0 r) == i)

noMatchTest = makeTest assertNoMatch
matchTest = makeTest assertMatch

tests = TestList [ noMatchTest (-1) [1, 2, 3]
                 , noMatchTest (-7) [1, 2, 3]
                 , noMatchTest 7 [1, 2, 3]
                 , matchTest 0 [1, 2, 3]
                 , matchTest 1 [1, 2, 3]
                 , matchTest 2 [1, 2, 3]
                 , matchTest 3 [1, 2, 3]
                 , matchTest 4 [1, 2, 3]
                 , matchTest 5 [1, 2, 3]
                 , matchTest 6 [1, 2, 3]
                 , matchTest 2845056 [355104, 476077, 476303, 224658, -17532, -183480, -286788, 238271, 231845, -227454, 226199, 105438, 316870, 353652, 173563, 244958, 367896, 105046, 495797, 447209, 397810, -394348, 242527, 17532, -57224, -38084, 82375, 445376, -297793, 368660, -65413, 96325, -472195, -23826, -113982, -355574, 331821]
                 ]
main = do
  runTestTT tests

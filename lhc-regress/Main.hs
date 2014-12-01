{-# LANGUAGE TemplateHaskell #-}
module Main where
import Test.Framework (defaultMain, testGroup, Test)

import UnitTests

main :: IO ()
main = defaultMain tests
  where tests = [ testGroup "Unit tests" unitTests
                , testGroup "Properties" propertyList
                ]

propertyList :: [Test]
propertyList = []

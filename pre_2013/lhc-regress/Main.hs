{-# LANGUAGE TemplateHaskell #-}
module Main where
import Test.Framework (defaultMain, testGroup, Test)

import Properties
import UnitTests
import THUtil

main :: IO ()
main = defaultMain tests
  where tests = [ testGroup "Unit tests" unitTests
                , testGroup "Properties" propertyList
                ]

propertyList :: [Test]
propertyList = $(props)

module Properties where
import Test.QuickCheck
import Data.Monoid
import Control.Monad

import Grin.Types
import Grin.HPT.Environment as Env

-- Necessary arbitrary instances

instance Arbitrary Rhs where
    arbitrary = do rhsValues <- arbitrary
                   return $ mconcat (map singleton rhsValues)

instance Arbitrary RhsValue where
    arbitrary = oneof [ liftM3 Extract arbitrary arbitrary arbitrary
                      , liftM2 ExtractVector arbitrary arbitrary
                      , liftM Eval arbitrary
                      , liftM2 Env.Update arbitrary arbitrary
                      , liftM2 Apply arbitrary arbitrary
                      , liftM2 PartialApply arbitrary arbitrary
                      , liftM Ident arbitrary
                      , liftM Fetch arbitrary
                      , return Base
                      , liftM Heap arbitrary
                      , sized $ \n -> liftM4 Tag arbitrary arbitrary arbitrary (resize (n `div` 2) arbitrary)
                      , sized $ \n -> liftM VectorTag (resize (n `div` 2) arbitrary)
                      ]

instance Arbitrary Renamed where
    arbitrary = liftM Anonymous arbitrary

instance Arbitrary NodeType where
    arbitrary = elements [ ConstructorNode
                         , FunctionNode ]


-- The following portion of the file is dedicated to QuickCheck-based tests
-- All tests should follow the naming convention:
-- 
--    prop_TestGroup_TestName = ...
--
-- where 'TestGroup' is the Group the test is in, and 'TestName' is the name
-- of the specific test.

prop_HPT_isSubsetOf a b = a `isSubsetOf` b == (b == (a `mappend` b))

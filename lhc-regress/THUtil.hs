{-# LANGUAGE TemplateHaskell #-}
-- | This file contains some code to extract
-- the properties out of @Properties.hs@ and turn them
-- into a list, which we run with 'Test.Framework'.
--
-- It parses @Properties.hs@ with haskell-src-exts,
-- iterates over the top-level declarations and picks out
-- anything with the "prop_" prefix, and puts that into a
-- list which is then run with QuickCheck2 and the
-- test-framework package.
-- TODO: better module name?
module THUtil
( props -- :: ExpQ
) where
import qualified Data.Map as M
import Data.List hiding (group)
import Language.Haskell.TH hiding (Match)
import Language.Haskell.Exts hiding (listE, name)
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import System.IO.Unsafe

-- This parses all the tests in Properties.hs and returns a Map where the key
-- is the test group, and the value the tests in that group
allProps :: M.Map String [String]
allProps = unsafePerformIO $ do
  f <- parseFile $ "lhc-regress/Properties.hs"
  case f of
    ParseOk (Module _ _ _ _ _ _ d) -> do
      let propNames = map (span (/='_')) $ map (drop 5) $ map declName $ filter isProp d
          entries = foldr (\(group, name) -> M.insertWith (++) group [(drop 1 name)]) 
                     M.empty propNames
      return entries
    ParseFailed loc e -> error (unwords [e, show loc])

isProp :: Decl -> Bool
isProp d@(FunBind _) = "prop_" `isPrefixOf` (declName d)
isProp _ = False

declName :: Decl -> String
declName (FunBind (Match _ (Ident n) _ _ _ _:_)) = n
declName _ = undefined

-- | This is a template haskell splice which will return a list of tests, along with
-- their groups, that are found in 'Properties.hs' at compile time.
-- 
-- Tests in 'Properties.hs' should follow the naming convention:
--    @prop_GroupName_TestName = ...@
-- so that the parser can get things right. Otherwise bad things will probably happen.
props :: ExpQ
props = listE $ map mk $ M.toList allProps
  where mk (g,t) = [| testGroup $(litE $ stringL g) $(listE $ map (mkProp g) t) |]
        mkProp g t = let fullname = "prop_" ++ g ++ "_" ++ t 
                     in [| testProperty $(litE $ stringL t) $(varE $ mkName fullname) |]

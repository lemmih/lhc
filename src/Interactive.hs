module Interactive where

import Interpreter
import qualified Language.Haskell.Crux as Crux
import qualified Data.Map as Map
import Data.IORef
import Data.List

toGCode :: Crux.Module -> IO Context
toGCode m = do
    ctx <- sequence [ do ref <- newIORef (ThunkClosure (Closure Map.empty gcode))
                         return (name, ref)
                    | (name, gcode) <- decls ]
    return $ Map.fromList ctx
  where
    decls = map declToGCode (Crux.cruxDecls m)

declToGCode :: Crux.Declaration -> (Name, GCode)
declToGCode (Crux.Declaration _ty n e) = (toName n, exprToGCode e)

exprToGCode :: Crux.Expr -> GCode
exprToGCode e =
  case e of
    Crux.Var var -> Var (toName $ Crux.varName var) []
    Crux.Lam vars rest -> Lam [] (map (toName . Crux.varName) vars) $ exprToGCode rest
    _ -> Con "Missing" []
-- data Expr
--     = Var Variable
--     | Con Variable
--     | UnboxedTuple [Expr]
--     | Lit Literal
--     | WithExternal Variable Variable String [Expr] Expr Expr
--     | ExternalPure Variable String [Expr] Expr
--     | App Expr Expr
--     | Lam [Variable] Expr
--     | Let LetBind Expr
--     | LetStrict Variable Expr Expr
--     | Case Expr Variable (Maybe Expr) [Alt]
--     | Convert Expr Type
--     | Cast
--     deriving ( Show, Eq, Generic )

toName :: Crux.Name -> Name
toName (Crux.Name m i 0) = intercalate "." m ++ "." ++ i
toName (Crux.Name m i u) = intercalate "." m ++ "." ++ i ++ "_" ++ show u

interpret :: Crux.Module -> IO ()
interpret m = do
  printContext 0 =<< toGCode m

{-# LANGUAGE ParallelListComp #-}
module Interactive where

import           Data.IORef
import           Data.List
import qualified Data.Map              as Map
import           Interpreter
import qualified Language.Haskell.Crux as Crux

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
    _ | (Crux.Var var, apps) <- collectApps [] e -> bindExprs apps $ \names ->
      Var (toName $ Crux.varName var) names
    _ | (Crux.Con con, apps) <- collectApps [] e -> bindExprs apps $ \names ->
      Con (toName $ Crux.varName con) names
    -- Crux.Var var -> Var (toName $ Crux.varName var) []
    Crux.Lam vars rest -> Lam [] (map (toName . Crux.varName) vars) $ exprToGCode rest
    Crux.Lit lit -> Lit (litToGCode lit)
    Crux.Case scrut var mbDef alts ->
      let scrutName = varName var in
      LetStrict scrutName
        (exprToGCode scrut)
        (Case scrutName (fmap exprToGCode mbDef)
          (map altToGCode alts))
    Crux.Let (Crux.NonRec var e) body ->
      Let (varName var) [] (exprToGCode e) (exprToGCode body)
    Crux.Convert body _ty ->
      LetStrict "tmp_arg" (exprToGCode body) $
      Var "_cast" ["tmp_arg"]
    Crux.WithExternal outV outS cName args st cont ->
      let names = [ "tmp_"++show nth | nth <- [1 .. length args] ] in
      foldr (.) id [ Let nth [] (exprToGCode arg) | arg <- args | nth <- names ] $
      LetStrict (varName outS) (exprToGCode st) $
      LetStrict (varName outV) (External cName names) $
      exprToGCode cont
    Crux.ExternalPure outV cName args cont -> bindExprs args $ \names ->
      LetStrict (varName outV) (External cName names) $
      exprToGCode cont
    Crux.UnboxedTuple exprs -> bindExprs exprs $ \names ->
      Con "" names
    _ -> Con "Missing" []
  where
    bindExprs exprs rest =
      let names = [ "tmp_"++show nth | nth <- [1 .. length exprs] ] in
      foldr (.) id [ Let nth [] (exprToGCode e) | e <- exprs | nth <- names ] $
      rest names
    collectApps acc (Crux.App a b) = collectApps (b:acc) a
    collectApps acc e = (e, acc)

altToGCode :: Crux.Alt -> Alt
altToGCode (Crux.Alt (Crux.ConPat con vars) branch) =
  Alt (ConPattern (varName con) $ map varName vars) (exprToGCode branch)
altToGCode (Crux.Alt (Crux.LitPat lit) branch) =
  Alt (LitPattern (litToGCode lit)) (exprToGCode branch)
altToGCode (Crux.Alt (Crux.UnboxedPat vars) branch) =
  Alt (ConPattern "" $ map varName vars) (exprToGCode branch)

litToGCode :: Crux.Literal -> Literal
litToGCode Crux.LitVoid = LiteralI64 0
litToGCode (Crux.LitI64 n) = LiteralI64 (fromIntegral n)
litToGCode (Crux.LitString s) = LiteralString s
litToGCode _ = LiteralI64 0

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

varName :: Crux.Variable -> Name
varName = toName . Crux.varName

toName :: Crux.Name -> Name
toName (Crux.Name m i 0) = intercalate "." m ++ "." ++ i
toName (Crux.Name m i u) = intercalate "." m ++ "." ++ i ++ "_" ++ show u

interpret :: Crux.Module -> IO ()
interpret m = do
  ctx <- toGCode m
  finalize ctx
  printContext 0 ctx
  putStrLn "\n\nRunning:"
  runThunk ctx "Main.entrypoint"

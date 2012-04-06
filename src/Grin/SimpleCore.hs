{-# LANGUAGE TemplateHaskell #-}
{-
SimpleCore is a subset of External Core that can more easily be translated to Grin.

We do
 * Hoist out local functions (lets and lambdas)

-}
module Grin.SimpleCore
  ( SimpleModule(..)
  , ModuleIdent
  , moduleIdent
  , SimpleType(..)
  , SimpleEnum(..)
  , Ty(..)
  , SimpleDef(..)
  , SimpleExp(..)
  , simpleDefArity
  , Alt(..)
  , Lit(..)
  , coreToSimpleCore
  ) where

import Grin.Types (Variable)
import Grin.SimpleCore.Types
import Language.Core (Tdef,Vdef(..))
-- TODO: The Language.Core library uses parsec and is fairly slow. We could write
-- TODO: a faster version using Happy.
import qualified Language.Core as Core

import CompactString

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Monad.RWS
import Data.List
import Data.Maybe

import Traverse

{-
The data structures for the simplified core is similar to plain core with a few exceptions:
 * Lets always have the form 'let x = fn a b c in'
 * Lambdas have been removed
 * Application chains have been converted to lists of arguments.
-}








coreToSimpleCore :: Core.Module -> SimpleModule
coreToSimpleCore (Core.Module (pkgname,modname) tdefs vdefs)
    = let (_, simpleDefs) = execRWS (mapM_ vdefToSimpleDef allDefs) emptyScope 0
      in SimpleModule { modulePackage = L.unpack pkgname
                      , moduleName    = L.unpack modname
                      , moduleNewTypes = tdefsToNewTypes tdefs
                      , moduleTypes   = concatMap tdefToSimpleTypes tdefs
                      , moduleEnums   = mapMaybe tdefToSimpleEnum tdefs
                      , moduleDefs    = simpleDefs }
    where allDefs = concatMap (\x -> case x of Core.Nonrec d -> [d]; Core.Rec ds -> ds) vdefs
          emptyScope = Scope { currentScope  = Map.empty
                             , currentModule = (pkgname, modname)
                             , currentContext= Lazy }

tdefsToNewTypes :: [Core.Tdef] -> Map.Map CompactString ([Tvar], Ty)
tdefsToNewTypes tdefs
    = Map.fromList [ (qualToCompact qual, ([ fromLazyByteString tvar | (tvar,_kind) <- tbinds ], tyToSimpleTy ty))
                   | Core.Newtype qual _ tbinds ty <- tdefs ]

tdefToSimpleTypes :: Core.Tdef -> [SimpleType]
tdefToSimpleTypes (Core.Data _ tbinds cdefs) = map (cdefToSimpleType (length tbinds)) cdefs
tdefToSimpleTypes (Core.Newtype{}) = []

cdefToSimpleType :: Int -> Core.Cdef -> SimpleType
cdefToSimpleType tyArity (Core.Constr qual _ tys) = SimpleType { simpleTypeName = qualToCompact qual
                                                               , simpleTypeArity = length tys
                                                               , simpleTypeTyArity = tyArity }
cdefToSimpleType _ (Core.GadtConstr{}) = error "GADTs aren't yet supported!"

tdefToSimpleEnum :: Core.Tdef -> Maybe SimpleEnum
tdefToSimpleEnum (Core.Data qual [] cdefs)
    = Just (SimpleEnum { simpleEnumName = qualToCompact qual
                       , simpleEnumMembers = mapMaybe cdefToSimpleEnum cdefs })
tdefToSimpleEnum _ = Nothing

cdefToSimpleEnum :: Core.Cdef -> Maybe CompactString
cdefToSimpleEnum (Core.Constr qual [] []) = Just (qualToCompact qual)
cdefToSimpleEnum _ = Nothing

sdefDeps :: Core.Exp -> [(String, String)]
sdefDeps exp
    = let free = Set.toList $ freeVariables exp
      in nub [ (L.unpack pkg, L.unpack mod)
             | qual@(pkg,mod,_ident) <- free
             , not (L.null pkg)
             , not (L.null mod) ]


isPrimitiveQual (pkg,mod,_ident)
    = pkg == L.pack "ghczmprim" && mod == L.pack "GHCziPrim"

isEnumPrimitive (_pkg, _mod, ident)
    = ident == L.pack "tagToEnumzh" || ident == L.pack "dataToTagzh"


--type ScopeEnv = Map.Map (Core.Qual Core.Id) Renamed
data Scope = Scope { currentScope :: Map.Map (Core.Qual Core.Id) Core.Ty
                   , currentModule :: (Core.Pkgname, Core.Mname)
                   , currentContext :: Context }
data Context = Strict | Lazy deriving Eq
type M = RWS Scope [SimpleDef] Int

setContext :: Context -> M a -> M a
setContext cxt
    = local (\scope -> scope{ currentContext = cxt })

askContext :: M Context
askContext = asks currentContext

vdefToSimpleDef :: Core.Vdef -> M ()
vdefToSimpleDef vdef
    = let (args, tvars, body) = splitExp (vdefExp vdef)
      in bindVariables args $ vdefToSimpleDef' (qualToCompact (vdefName vdef)) (map fromLazyByteString tvars) [ qualToCompact var | (var,_ty) <- args ] body

vdefToSimpleDef' :: CompactString -> [Tvar] -> [CompactString] -> Core.Exp -> M ()
vdefToSimpleDef' name tyArgs args body
    = do body' <- expToSimpleExp body
         tell [SimpleDef { simpleDefName = name
                         , simpleDefArgs = args
                         , simpleDefTyArgs = tyArgs
                         , simpleDefBody = body'
                         , simpleDefDeps = sdefDeps body }]

expToSimpleExp :: Core.Exp -> M SimpleExp
expToSimpleExp (Core.App (Core.Appt (Core.Var qual@(_pkg,_mod,ident)) t) (Core.Var var))
    | isPrimitiveQual qual && isEnumPrimitive qual
    = return $ EnumPrimitive (qualToCompact (L.empty, L.empty, ident)) (qualToCompact var) (tyToSimpleTy t)
expToSimpleExp (Core.Var qual@(pkg,mod,ident)) | isPrimitiveQual qual
    = return $ Primitive (qualToCompact (L.empty, L.empty, ident))
expToSimpleExp (Core.Var var)  = do isUnboxed <- varIsStrictPrimitive var
                                    return $ Var (qualToCompact var) isUnboxed
expToSimpleExp (Core.Dcon con) = return $ Dcon (qualToCompact con)
expToSimpleExp (Core.Lit lit)  = return $ Lit $ fromCoreLit lit
expToSimpleExp e@Core.App{}    = appToSimpleExp e
expToSimpleExp e@Core.Appt{}   = appToSimpleExp e
expToSimpleExp (Core.Lamt _ e) = expToSimpleExp e
-- We remove lambdas by translating them to let expressions.
expToSimpleExp exp@(Core.Lam (var,ty) _)
    = do newVar <- uniqueQual var
         let def     = Vdef { vdefLocal = False
                            , vdefName  = newVar
                            , vdefType  = Core.Tvar $ error "unknown type" -- Urk.
                            , vdefExp   = exp }
         expToSimpleExp (Core.Let (Core.Nonrec def) (Core.Var newVar))
expToSimpleExp (Core.Let (Core.Nonrec def) e) | defIsStrictPrimitive def
    = bindDef def $
      return (LetStrict (qualToCompact (vdefName def))) `ap` expToSimpleExp (vdefExp def) `ap` expToSimpleExp e
expToSimpleExp (Core.Let (Core.Nonrec def) e)
    = bindDef def $
      do (name, toplevelName, tvars, args, arity) <- lambdaLift def
         return (Let name toplevelName tvars args arity) `ap` expToSimpleExp e
expToSimpleExp (Core.Let (Core.Rec defs) e)
    = bindDefs defs $ return LetRec `ap` mapM lambdaLift defs `ap` expToSimpleExp e
expToSimpleExp (Core.Case e bind ty [Core.Adefault cond]) | typeIsStrictPrimitive (snd bind)
    = bindVariable bind $
      return (LetStrict (qualToCompact (fst bind))) `ap` setContext Strict (expToSimpleExp e) `ap` expToSimpleExp cond
expToSimpleExp (Core.Case e bind ty alts)
    = bindVariable bind $
      do e' <- setContext Strict $ expToSimpleExp e
         alts' <- mapM altToSimpleAlt alts
         let constr = if typeIsStrictPrimitive (snd bind) then CaseStrict else Case
         return $ constr e' (qualToCompact $ fst bind) (tyToSimpleTy (snd bind)) alts'
expToSimpleExp (Core.Cast e _ty) = expToSimpleExp e
expToSimpleExp (Core.External target conv ty) = return $ External target conv (tyToFFITypes ty)
expToSimpleExp (Core.DynExternal conv ty)     = return $ DynExternal conv (tyToFFITypes ty)
expToSimpleExp (Core.Label label)             = return $ Label label
expToSimpleExp (Core.Note note e)             = {- return (Note note) `ap` -} expToSimpleExp e

appToSimpleExp e
    = do let (f, coreTys, args) = collectApps e
             tys = map tyToSimpleTy coreTys
         e' <- expToSimpleExp f
         cxt <- askContext
         case e' of
           Primitive{} -> return (App e' tys) `ap` mapM expToSimpleExp args
           External{}  -> return (App e' tys) `ap` mapM expToSimpleExp args
           _  | cxt == Strict -> return (App e' tys) `ap` mapM expToSimpleExp args
              | otherwise     -> return (App e' tys) `ap` mapM lambdaLiftExp args


tyToSimpleTy :: Core.Ty -> Ty
tyToSimpleTy (Core.Tcon con) = Tcon (qualToCompact con)
tyToSimpleTy (Core.Tvar tvar) = Tvar (fromLazyByteString tvar)
tyToSimpleTy (Core.Tapp a b) = Tapp (tyToSimpleTy a) (tyToSimpleTy b)
tyToSimpleTy (Core.Tarrow a b) = Tarrow (tyToSimpleTy a) (tyToSimpleTy b)
tyToSimpleTy (Core.Tforall (tvar,_kind) ty) = Tforall (fromLazyByteString tvar) (tyToSimpleTy ty)
tyToSimpleTy ty = error $ "Invalid enum type: " ++ show ty

tyToFFITypes :: Core.Ty -> [FFIType]
tyToFFITypes (Core.Tarrow (Core.Tcon con) rest)
    = conToFFIType con : tyToFFITypes rest
tyToFFITypes (Core.Tarrow (Core.Tapp (Core.Tcon state) (Core.Tcon realworld)) ret)
    | state == statezh && realworld == theRealWorld
    = case ret of
        Core.Tapp (Core.Tcon tuple) (Core.Tapp (Core.Tcon state) (Core.Tcon realworld))
            | tuple == z1h && state == statezh && realworld == theRealWorld
          -> [UnitType]
        Core.Tapp (Core.Tapp (Core.Tcon tuple) (Core.Tapp (Core.Tcon state) (Core.Tcon realworld))) (Core.Tcon con)
            | tuple == z2h && state == statezh && realworld == theRealWorld
          -> [conToFFIType con]
        _ -> [InvalidType]
    where z1h = mkPrimQual "Z1H"
          z2h = mkPrimQual "Z2H"
          statezh = mkPrimQual "Statezh"
          theRealWorld = mkPrimQual "RealWorld"
tyToFFITypes ty = [InvalidType] -- error $ "Unrecognized ffi type: " ++ show ty

mkPrimQual name
    = (L.pack "ghczmprim", L.pack "GHCziPrim", L.pack name)

conToFFIType :: Core.Qual Core.Tcon -> FFIType
conToFFIType con
    | con == wordzh = UnsignedType
    | con == intzh  = SignedType
    | con == addrzh = PointerType
    | otherwise     = InvalidType
    where wordzh = mkPrimQual "Wordzh"
          intzh  = mkPrimQual "Intzh"
          addrzh = mkPrimQual "Addrzh"


defIsStrictPrimitive :: Vdef -> Bool
defIsStrictPrimitive
    = typeIsStrictPrimitive . vdefType

-- FIXME: This function is incomplete.
typeIsStrictPrimitive :: Core.Ty -> Bool
typeIsStrictPrimitive ty
    = case ty of
        Core.Tcon con -> isPrimitiveQual con
        Core.Tapp a b -> typeIsStrictPrimitive a
        _ -> False

altToSimpleAlt :: Core.Alt -> M Alt
altToSimpleAlt (Core.Acon con _tbinds vbinds e) = let bs = map fst vbinds
                                                  in return (Acon (qualToCompact con) (map qualToCompact bs)) `ap` bindVariables vbinds (expToSimpleExp e)
altToSimpleAlt (Core.Alit lit e)                = return (Alit $ fromCoreLit lit) `ap` expToSimpleExp e
altToSimpleAlt (Core.Adefault e)                = return Adefault `ap` expToSimpleExp e




fromCoreLit :: Core.Lit -> Lit
fromCoreLit (Core.Lint int _ty)           = Lint int
fromCoreLit (Core.Lrational rational _ty) = Lrational rational
fromCoreLit (Core.Lchar char _ty)         = Lchar char
fromCoreLit (Core.Lstring string _ty)     = Lstring string

{-
First we make the scope explicit:

let a = \x -> x + b in
->
let a b = \x -> x + b

Then we push the function to the top level:

top-level: let_a b = \x -> x + b
let a = let_a b

We have to do this because functions are represented with tags instead of
function pointers.



fn n = if n == 0 then [] else Cons (ptr!!n) (fn (n+1))

let_fn ptr n = let_s fn = let_fn ptr
               in x + fn 0


a = x : a

let_a a x = : x a

-}
lambdaLift :: Core.Vdef -> M (CompactString, CompactString, [Ty], [Variable], Int)
lambdaLift vdef@Vdef{vdefName = (_pkg,_mod,ident), vdefExp = exp}
    = do (pkg,mod) <- asks currentModule
         scope <- asks currentScope
         unique <- newUnique
         let allFreeVars = freeVariables exp `Set.intersection` Map.keysSet scope
             tyScope     = Set.toList $ freeTypeVariables exp
             isRecursive = vdefName vdef `Set.member` allFreeVars
             lambdaScope = Set.toList $ if isCAF then allFreeVars else Set.delete (vdefName vdef) allFreeVars
             isCAF = null args
             (args, coreTvars, body) = splitExp exp
             tvars = map fromLazyByteString (coreTvars ++ tyScope)
         lambdaScopeTyped <- mapM (\var -> do t <- varType var; return (var, t)) lambdaScope
         let
             realArgs = map qualToCompact (lambdaScope ++ map fst args)
             toplevelName = (pkg,mod,L.pack "@lifted@_" `L.append` ident `L.append` L.pack (show unique))
             selfDef = Core.Case (foldl Core.App (Core.Var toplevelName) (map Core.Var lambdaScope))
                                 (vdefName vdef,vdefType vdef)
                                 (vdefType vdef)
                                 [Core.Adefault body]

         bindVariables (lambdaScopeTyped ++ args) $
           if isCAF || not isRecursive
             then vdefToSimpleDef' (qualToCompact toplevelName) tvars realArgs body
             else vdefToSimpleDef' (qualToCompact toplevelName) tvars realArgs selfDef

         return ( qualToCompact (vdefName vdef)
                , qualToCompact toplevelName
                , map (Tvar . fromLazyByteString) tyScope
                , map qualToCompact lambdaScope
                , length realArgs )

lambdaLiftExp :: Core.Exp -> M SimpleExp
lambdaLiftExp e@Core.Var{} = expToSimpleExp e
lambdaLiftExp e@Core.Lit{} = expToSimpleExp e
lambdaLiftExp e@Core.Dcon{} = expToSimpleExp e
--lambdaLiftExp (Core.Appt e _t) = lambdaLiftExp e
lambdaLiftExp e@Core.App{} | (Core.Var qual, _tys, _args) <- collectApps e
                           , isPrimitiveQual qual
    = expToSimpleExp e
lambdaLiftExp exp
    = do (pkg, mod) <- asks currentModule
         scope <- asks currentScope
         unique <- newUnique
         let allFreeVars = freeVariables exp `Set.intersection` Map.keysSet scope
             lambdaScope = Set.toList allFreeVars
             tyScope     = Set.toList $ freeTypeVariables exp
             tvars       = map fromLazyByteString tyScope
         lambdaScopeTyped <- mapM (\var -> do t <- varType var; return (var, t)) lambdaScope
         let
             realArgs = map qualToCompact lambdaScope
             toplevelName = (pkg,mod,L.pack "@lifted_exp@_" `L.append` L.pack (show unique))

         bindVariables (lambdaScopeTyped) $
           vdefToSimpleDef' (qualToCompact toplevelName) tvars realArgs exp

         return $ App (Var (qualToCompact toplevelName) False) [ Tvar tvar | tvar <- tvars ] [ Var (qualToCompact arg) False | arg <- lambdaScope ]
         {-return ( qualToCompact toplevelName
                , map qualToCompact lambdaScope
                , length realArgs )-}


freeVariables :: Core.Exp -> Set.Set (Core.Qual Core.Id)
freeVariables (Core.Var qual)                  = Set.singleton qual
freeVariables (Core.Dcon qual)                 = Set.singleton qual
freeVariables (Core.Lam (var,_ty) e)           = Set.delete var $ freeVariables e
freeVariables (Core.Let (Core.Nonrec def) e)   = freeVariables (Core.Let (Core.Rec [def]) e)
freeVariables (Core.Let (Core.Rec defs) e)     = Set.unions (freeVariables e : map (freeVariables . vdefExp) defs) `Set.difference` bound
    where bound = Set.fromList (map vdefName defs)
freeVariables (Core.Case e (var,_ty) _ alts)
    = freeVariables e `Set.union` Set.delete var (Set.unions (map freeVariablesAlt alts))
freeVariables e = tsum freeVariables e

freeVariablesAlt :: Core.Alt -> Set.Set (Core.Qual Core.Id)
freeVariablesAlt (Core.Acon con _tbinds vbinds e)
    = Set.insert con $ freeVariables e `Set.difference` Set.fromList [ var | (var, _ty) <- vbinds ]
freeVariablesAlt (Core.Alit _lit e)
    = freeVariables e
freeVariablesAlt (Core.Adefault e)
    = freeVariables e

freeTypeVariables :: Core.Exp -> Set.Set Core.Tvar
freeTypeVariables exp
    = case exp of
        Core.Lamt (ty,_kind) e         -> Set.delete ty $ freeTypeVariables e
        Core.Let (Core.Nonrec def) e   -> freeTypeVariables (Core.Let (Core.Rec [def]) e)
        Core.Let (Core.Rec defs) e     -> Set.unions (freeTypeVariables e : map (freeTypeVariables . vdefExp) defs)
        Core.Appt e ty                 -> worker ty `Set.union` freeTypeVariables e
        Core.Case _ (_bind, ty) _ _    -> worker ty `Set.union` tsum freeTypeVariables exp
        _other                         -> tsum freeTypeVariables exp
    where worker (Core.Tvar tvar) = Set.singleton tvar
          worker (Core.Tapp a b) = worker a `Set.union` worker b
          worker (Core.Tarrow a b) = worker a `Set.union` worker b
          worker _ = Set.empty





bindVariable :: (Core.Qual Core.Id, Core.Ty) -> M a -> M a
bindVariable (var, ty)
    = local $ \scope -> scope{ currentScope = Map.insert var ty (currentScope scope)}

bindVariables :: [(Core.Qual Core.Id, Core.Ty)] -> M a -> M a
bindVariables [] = id
bindVariables (x:xs) = bindVariable x . bindVariables xs

bindDef :: Vdef -> M a -> M a
bindDef def
    = bindVariable (vdefName def, vdefType def)

bindDefs :: [Vdef] -> M a -> M a
bindDefs [] = id
bindDefs (x:xs) = bindDef x . bindDefs xs

varIsStrictPrimitive :: Core.Qual Core.Id -> M Bool
varIsStrictPrimitive var
    = asks $ \st -> case Map.lookup var (currentScope st) of
                      Nothing -> False
                      Just ty -> typeIsStrictPrimitive ty

varType :: Core.Qual Core.Id -> M Core.Ty
varType var
    = asks $ \st -> Map.findWithDefault errMsg var (currentScope st)
    where errMsg = error $ "Couldn't find type for: " ++ show var

newUnique :: M Int
newUnique = do u <- get
               put $! u+1
               return u

uniqueQual :: Core.Qual Core.Id -> M (Core.Qual Core.Id)
uniqueQual (pkg,mod,ident)
    = do u <- newUnique
         return (pkg, mod, ident `L.append` L.pack (show u))


splitExp :: Core.Exp -> ([(Core.Qual Core.Id,Core.Ty)], [Core.Tvar], Core.Exp)
splitExp (Core.Lam b exp)
    = let (args, tvars, body) = splitExp exp
      in (b:args, tvars, body)
splitExp (Core.Lamt (tvar,_kind) exp)
    = let (args, tvars, body) = splitExp exp
      in (args, tvar:tvars, body)
splitExp (Core.Note _ exp) = splitExp exp
splitExp exp = ([], [], exp)


collectApps ::Core.Exp -> (Core.Exp, [Core.Ty], [Core.Exp])
collectApps = worker [] []
    where worker accTy acc (Core.App a b)
              = worker accTy (b:acc) a
          worker accTy acc (Core.Appt a ty)
              = worker (ty:accTy) acc a
          worker accTy acc (Core.Note _ a)
              = worker accTy acc a
          worker accTy acc (Core.Cast e _)
              = worker accTy acc e
          worker accTy acc a
              = (a,accTy, acc)







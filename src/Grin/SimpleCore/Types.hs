{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wwarn #-}
module Grin.SimpleCore.Types where

import CompactString
import Traverse
import qualified Language.Core as Core
import qualified Data.Map as Map
import Control.Monad

import Data.Binary
import Data.DeriveTH

data SimpleModule
    = SimpleModule { modulePackage  :: String
                   , moduleName     :: String
                   , moduleNewTypes :: SimpleNewTypes
                   , moduleTypes    :: [SimpleType]
                   , moduleEnums    :: [SimpleEnum]
                   , moduleDefs     :: [SimpleDef]
                   }

type SimpleNewTypes = Map.Map CompactString ([Tvar], Ty)

type ModuleIdent = (String,String)
moduleIdent mod = (modulePackage mod, moduleName mod)

data SimpleType
    = SimpleType { simpleTypeName  :: CompactString
                 , simpleTypeArity :: Int
                 , simpleTypeTyArity :: Int
                 } deriving (Eq)

data SimpleEnum
    = SimpleEnum { simpleEnumName :: CompactString
                 , simpleEnumMembers :: [CompactString]
                 }

data SimpleDef
    = SimpleDef { simpleDefName :: CompactString
                , simpleDefArgs :: [CompactString]
                , simpleDefTyArgs :: [Tvar]
                , simpleDefBody :: SimpleExp
                , simpleDefDeps :: [(String,String)]
                }
simpleDefArity :: SimpleDef -> Int
simpleDefArity = length . simpleDefArgs

data SimpleExp
    = Var CompactString Bool
    | Primitive CompactString
    | EnumPrimitive CompactString CompactString Ty
    | Dcon CompactString
    | Lit Lit
    | App SimpleExp [Ty] [SimpleExp]
    | Let CompactString CompactString [Ty] [CompactString] Int SimpleExp
    | LetRec [(CompactString, CompactString, [Ty], [CompactString], Int)] SimpleExp
    | LetStrict CompactString SimpleExp SimpleExp
    | Case SimpleExp CompactString Ty [Alt]
    | CaseStrict SimpleExp CompactString Ty [Alt]
    | External String String [FFIType]
    | DynExternal String [FFIType]
    | Label String
    | Note String SimpleExp

type Tvar = CompactString
data Ty = Tcon CompactString
        | Tvar Tvar
        | Tapp Ty Ty
        | Tarrow Ty Ty
        | Tforall Tvar Ty
    deriving ( Show, Eq, Ord )

data FFIType = UnsignedType | SignedType | PointerType | UnitType | InvalidType
    deriving ( Show, Eq, Ord )

data Alt
    = Acon CompactString [CompactString] SimpleExp
    | Alit Lit SimpleExp
    | Adefault SimpleExp

data Lit
    = Lint Integer
    | Lrational Rational
    | Lchar Char
    | Lstring String
    deriving (Show,Eq,Ord)

$(derive makeBinary ''Alt)
$(derive makeBinary ''Lit)
$(derive makeBinary ''Ty)
$(derive makeBinary ''FFIType)
$(derive makeBinary ''SimpleExp)
$(derive makeBinary ''SimpleDef)
$(derive makeBinary ''SimpleType)
$(derive makeBinary ''SimpleEnum)
$(derive makeBinary ''SimpleModule)


instance Traverse Core.Exp where
    tmapM fn exp = case exp of
        Core.Var{}  -> return exp
        Core.Dcon{} -> return exp
        Core.Lit{}  -> return exp
        Core.App a b -> return Core.App `ap` fn a `ap` fn b
        Core.Appt a t -> return Core.Appt `ap` fn a `ap` return t
        Core.Lam b e -> return (Core.Lam b) `ap` fn e
        Core.Lamt b e -> return (Core.Lamt b) `ap` fn e
        Core.Let vdefg e -> return Core.Let `ap` return vdefg `ap` fn e
        Core.Case e bind ty alts
          -> let mapAlt (Core.Acon qual tbinds vbinds e)
                     = return (Core.Acon qual tbinds vbinds) `ap` fn e
                 mapAlt (Core.Alit lit e)
                     = return (Core.Alit lit) `ap` fn e
                 mapAlt (Core.Adefault e)
                     = return Core.Adefault `ap` fn e
             in return Core.Case `ap` fn e `ap` return bind `ap` return ty `ap` mapM mapAlt alts
        Core.Cast e ty -> return Core.Cast `ap` fn e `ap` return ty
        Core.Note n e  -> return (Core.Note n) `ap` fn e
        Core.External{} -> return exp
        Core.DynExternal{} -> return exp
        Core.Label{} -> return exp

instance Traverse SimpleExp where
    tmapM fn exp
        = case exp of
            Var{}           -> return exp
            Primitive{}     -> return exp
            EnumPrimitive{} -> return exp
            Dcon{}          -> return exp
            Lit{}           -> return exp
            App exp tys args -> return App `ap` fn exp `ap` return tys `ap` mapM fn args
            Let localName topLevel tvars args arity e
              -> return (Let localName topLevel tvars args arity) `ap` fn e
            LetRec lst e
              -> return (LetRec lst) `ap` fn e
            LetStrict lst a b
              -> return (LetStrict lst) `ap` fn a `ap` fn b
            Case scrut bind ty alts
              -> return Case `ap` fn scrut `ap` return bind `ap` return ty `ap` mapM mapAlt alts
            CaseStrict scrut bind ty alts
              -> return CaseStrict `ap` fn scrut `ap` return bind `ap` return ty `ap` mapM mapAlt alts
            External{}    -> return exp
            DynExternal{} -> return exp
            Label{}       -> return exp
            Note note e   -> return (Note note) `ap` fn e
        where mapAlt (Acon con vbinds e) = return (Acon con vbinds) `ap` fn e
              mapAlt (Alit lit e)        = return (Alit lit) `ap` fn e
              mapAlt (Adefault e)        = return Adefault `ap` fn e


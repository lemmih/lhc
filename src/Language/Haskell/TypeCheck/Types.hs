module Language.Haskell.TypeCheck.Types where

import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
import Language.Haskell.Exts.SrcLoc
import qualified Text.PrettyPrint.ANSI.Leijen as Doc
import Data.Binary

import Language.Haskell.Scope ( GlobalName(..), QualifiedName(..) )

-- Type variables are uniquely identified by their name and binding point.
-- The binding point is not enough since ty vars can be bound at an implicit
-- forall.
data TcVar = TcVar String SrcSpanInfo
    deriving ( Show, Eq )

data TcMetaVar = TcMetaRef String (IORef (Maybe TcType))
instance Show TcMetaVar where
    show (TcMetaRef name _) = name
instance Eq TcMetaVar where
    TcMetaRef _ r1 == TcMetaRef _ r2 = r1==r2

instance Binary TcMetaVar where
    put (TcMetaRef name _) = put name
    -- put = error "Binary.put: TcMetaVar"
    get = do
        name <- get
        return $ TcMetaRef name (unsafePerformIO $ newIORef Nothing)
    -- get = error "Binary.get: TcMetaVar"

data TcType
    = TcForall [TcVar] (Qual TcType)
    | TcFun TcType TcType
    | TcApp TcType TcType
    -- Uninstantiated tyvar
    | TcRef TcVar
    | TcCon QualifiedName
    -- Instantiated tyvar
    | TcMetaVar TcMetaVar
    | TcUnboxedTuple [TcType]
    | TcUndefined
    deriving ( Show, Eq )

data Coercion
    = CoerceId
    | CoerceAbs [TcVar]
    | CoerceAp [TcMetaVar]
    deriving ( Show , Eq)

instance Doc.Pretty Coercion where
    pretty c =
        case c of
            CoerceId  ->
                Doc.text "id"
            CoerceAbs vars ->
                Doc.text "abs" Doc.<+> Doc.pretty vars
            CoerceAp metas ->
                Doc.text "ap" Doc.<+> Doc.pretty metas

instance Doc.Pretty TcType where
    pretty ty =
        case ty of
            TcForall [] ([] :=> t) ->
                Doc.pretty t
            TcForall vars qual ->
                Doc.text "∀" Doc.<+> Doc.hsep (map Doc.pretty vars) Doc.<>
                Doc.dot Doc.<+> Doc.pretty qual
            TcFun a b ->
                Doc.parens (Doc.pretty a) Doc.<+>
                Doc.text "→ " Doc.<+> Doc.pretty b
            TcApp a b ->
                Doc.pretty a Doc.<+> Doc.pretty b
            TcCon (QualifiedName m ident) ->
                Doc.text (m ++ "." ++ ident)
            TcRef var -> Doc.pretty var
            TcMetaVar meta ->
                Doc.pretty meta
            TcUnboxedTuple tys ->
                Doc.text "(#" Doc.<+>
                (Doc.hsep $ Doc.punctuate Doc.comma $ map Doc.pretty tys) Doc.<+>
                Doc.text "#)"
            TcUndefined ->
                Doc.red (Doc.text "undefined")

instance Doc.Pretty TcVar where
    pretty (TcVar ident _src) = Doc.text ident

instance Doc.Pretty TcMetaVar where
    pretty (TcMetaRef ident ref) = unsafePerformIO $ do
        mbTy <- readIORef ref
        case mbTy of
            Just ty -> return $ Doc.pretty ty
            Nothing -> return $ Doc.blue (Doc.text ident)
    --pretty (TcMetaRef ident _) = Doc.blue (Doc.text ident)

instance Doc.Pretty t => Doc.Pretty (Qual t) where
    pretty ([] :=> t) = Doc.pretty t
    pretty (quals :=> t) =
        Doc.parens (Doc.hsep $ Doc.punctuate Doc.comma $ map Doc.pretty quals) Doc.<+>
        Doc.text "⇒" Doc.<+> Doc.pretty t

instance Doc.Pretty Pred where
    pretty (IsIn _gname _t) = error "Pretty pred"

data Qual t = [Pred] :=> t
    deriving ( Show, Eq )
data Pred = IsIn GlobalName TcType
    deriving ( Show, Eq )

-- Uninstantiated type signature.
-- eg: forall a. Maybe a -- type of Nothing
-- eg: Int -- type of 10
--data Scheme = Scheme [TcVar] (Qual TcType)
--    deriving ( Show )

--data Typed = Typed TcType Origin



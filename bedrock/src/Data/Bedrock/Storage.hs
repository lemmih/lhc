{-# LANGUAGE PatternGuards #-}
module Data.Bedrock.Storage
    ( lowerAlloc ) where

import           Control.Applicative    (pure, (<$>), (<*>))
import           Control.Monad.State
import qualified Data.Map               as Map
import qualified Data.Set               as Set

import           Data.Bedrock
import           Data.Bedrock.Transform

-- Replace all occurrences of @alloc n with:
--
-- success := @GCAllocate n
-- case success of
--   0 -> prepareForGC current_scope
--   1 -> continue current_scope
--
-- preprateForGC scope =
--   GCBegin
--   GCMark all root pointers
--   GCEnd
--   continue marked_scope
lowerAlloc :: Gen ()
lowerAlloc = do
    fs <- gets (Map.elems . envFunctions)
    mapM_ transformFunction fs
    --mkMarkNode
    return ()

transformFunction :: Function -> Gen ()
transformFunction fn = do
    body <- transformBlock fn (fnBody fn)
    let fn' = fn{fnBody = body}
    pushFunction fn'

transformBlock :: Function -> Block -> Gen Block
transformBlock origin block =
    case block of
        Case scrut defaultBranch alternatives ->
            Case scrut
                <$> pure defaultBranch
                <*> mapM (transformAlternative origin) alternatives
        Bind binds simple rest ->
            transformExpresion origin binds simple =<<
                transformBlock origin rest
        Return{} ->
            return block
        Raise{} ->
            return block
        TailCall{} ->
            return block
        Invoke{} ->
            return block
        InvokeHandler{} ->
            return block
        Exit ->
            return block
        Panic{} ->
            return block

transformAlternative :: Function -> Alternative -> Gen Alternative
transformAlternative origin (Alternative pat block) =
    Alternative pat <$> transformBlock origin block

--gcMarkNodeName :: Name
--gcMarkNodeName = Name [] "mark_node" 0

--mkMarkNode :: Gen ()
--mkMarkNode = do
--    nodeDefs <- gets (nodes . envModule)
--    fns <- gets (functions . envModule)

--    node <- newVariable "node" Node

--    let markArg (var, markedVar) = Bind [markedVar] $
--            case variableType var of
--                NodePtr   -> GCMark var
--                Node      -> Application gcMarkNodeName [var]
--                Primitive -> Unit [RefArg var]

--    alts <- forM nodeDefs $ \(NodeDefinition name tys) -> do
--        args <- mapM (newVariable "arg") tys
--        markedArgs <- mapM (tagVariable "marked") args
--        let zippedArgs = zip args markedArgs
--        ret <- newVariable "ret" Node
--        return $
--            Alternative (NodePat (ConstructorName name) args) $
--            flip (foldr markArg) zippedArgs $
--            Bind [ret] (Unit [NodeArg (ConstructorName name) markedArgs]) $
--            Return [ret]

--    fnAlts <- forM fns $ \fn ->
--        forM [0 .. length (fnArguments fn)] $ \blanks -> do
--            let args = reverse . drop blanks . reverse $ fnArguments fn
--            markedArgs <- mapM (tagVariable "marked") args
--            ret <- newVariable "ret" Node
--            let retNode = NodeArg (FunctionName (fnName fn) blanks) markedArgs
--            return $ Alternative
--                (NodePat
--                    (FunctionName (fnName fn) blanks)
--                    (reverse . drop blanks . reverse $ fnArguments fn)) $
--                flip (foldr markArg) (zip args markedArgs) $
--                Bind [ret] (Unit [retNode]) $
--                Return [ret]

--    let body =
--            Case node Nothing (alts ++ concat fnAlts)

--    pushFunction Function
--        { fnName = gcMarkNodeName
--        , fnArguments = [ node ]
--        , fnResults = [ Node ]
--        , fnBody = body }

transformExpresion
    :: Function -> [Variable] -> Expression
    -> Block -> Gen Block
transformExpresion origin binds simple rest =
    case simple of
        Store nodeName args | [bind] <- binds -> do
            let size = 1 + length args
            hp <- newVariable "hp" NodePtr
            hp' <- newVariable "hp'" NodePtr
            tmp <- newVariable "tmp" Node
            return $
                Bind [hp] (ReadRegister "hp") $
                Bind [bind] (TypeCast hp) $
                Bind [tmp] (MkNode nodeName []) $
                Bind [] (Write hp 0 tmp) $
                foldr (\(nth, arg) -> Bind [] (Write hp nth arg))
                    (Bind [hp'] (Address hp size) $
                     Bind [] (WriteRegister "hp" hp') rest) 
                    (zip [1..] args)
        Alloc n -> do
            continueName <- tagName "with_mem" (fnName origin)
            divertName <- tagName "without_mem" (fnName origin)
            check <- newVariable "check" (Primitive IWord)
            let scope = Set.toList (freeVariables rest)
            markedScope <- mapM (tagVariable "marked") scope
            let continue = TailCall continueName scope
                divert = TailCall divertName scope
                zippedScope = zip scope markedScope
                markScope (var, markedVar) = Bind [markedVar] $
                    case variableType var of
                        NodePtr   -> GCMark var
                        Node{}    -> GCMarkNode var
                        -- Application gcMarkNodeName [var]
                        Primitive{}-> TypeCast var
                        -- This shouldn't really happen
                        StaticNode{} -> GCMarkNode var
                        FramePtr  -> GCMark var

                divertBody =
                    Bind [] GCBegin $
                    flip (foldr markScope) zippedScope $
                    -- gc_evacuate ptrs
                    Bind [] GCEnd $
                    TailCall continueName markedScope
            pushFunction Function
                { fnName = continueName
                , fnAttributes = []
                , fnArguments = scope
                , fnResults = []
                , fnBody = rest }
            pushFunction Function
                { fnName = divertName
                , fnAttributes = []
                , fnArguments = scope
                , fnResults = []
                , fnBody = divertBody }
            return $
                Bind [check] (GCAllocate n) $
                Case check Nothing
                    [ Alternative (LitPat (LiteralInt 0)) divert
                    , Alternative (LitPat (LiteralInt 1)) continue]
        _       -> return $ Bind binds simple rest















{- What needs to be done?

Make @alloc explicit.

Write barrier: Object A points to object B.
If object B is in a younger generation than A, add A to the remembered set.
Hm, is this right?


@alloc 2
  ==>
cond := @heapOverflow 2
case cond of
    0 -> continue
    1 -> do
        @gc_begin_collection
        newScope <-
            for each element in scope
                if primitive
                    return primitive
                if node
                    gc_mark_node node
                if ptr
                    gc_mark_ptr ptr
        @gc_finalize
        continue with newScope

gc_mark_ptr ptr =
    node := @fetch ptr
    newNode := gc_mark_node node
    @update ptr newNode
    @return ptr

-}
{- fixed space
gc_init = allocate fixed space
gc_begin = no-op
gc_mark = no-op
gc_end = no-op
gc_allocate = bump pointer
-}

{- Semi-space
gc_init = allocate buffer
gc_begin = prepare to-space
gc_mark = evac object
gc_end = scavenge to-space; free from-space
gc_allocate = bump pointer

evac object dst =
    copyObjectTo object dst
    limit += size of object
scavenge idx =
    if idx < limit
        obj <- fetch idx
        evac obj children
        scavenge (idx+sizeOf obj)
    else
        all done

-}

{- Generational semi-space
-}


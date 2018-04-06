module Data.Bedrock.Misc where

import Data.Bedrock

dropLast :: Int -> [a] -> [a]
dropLast n = reverse . drop n . reverse

takeLast :: Int -> [a] -> [a]
takeLast n = reverse . take n . reverse

newIDByType :: AvailableNamespace -> Type -> (Int, AvailableNamespace)
newIDByType ns ty =
    case ty of
        NodePtr      ->
            ( nsNextPointerId ns
            , incGlobal ns{nsNextPointerId = nsNextPointerId ns + 1})
        FramePtr      ->
            ( nsNextPointerId ns
            , incGlobal ns{nsNextPointerId = nsNextPointerId ns + 1})
        Node         ->
            ( nsNextNodeId ns
            , incGlobal ns{nsNextNodeId = nsNextNodeId ns + 1})
        StaticNode{} ->
            ( nsNextNodeId ns
            , incGlobal ns{nsNextNodeId = nsNextNodeId ns + 1})
        Primitive{}  -> newGlobalID ns
        IWord{}      -> newGlobalID ns
            {-( nsNextPrimitiveId ns
            , incGlobal ns{nsNextPrimitiveId = nsNextPrimitiveId ns + 1})-}
  where
    incGlobal thisNS = thisNS{ nsNextGlobalId = nsNextGlobalId thisNS + 1}

newGlobalID :: AvailableNamespace -> (Int, AvailableNamespace)
newGlobalID ns =
    ( nsNextGlobalId ns
    , ns{nsNextGlobalId = nsNextGlobalId ns + 1})



-- NB: Node pointers. Not pointers to external memory.
isPointerType :: Type -> Bool
isPointerType NodePtr  = True
isPointerType FramePtr = True
isPointerType _ = False

isNodeType :: Type -> Bool
isNodeType Node = True
isNodeType StaticNode{} = True
isNodeType _ = False


anyMemory :: MemAttributes
anyMemory = MemAttributes{ memConstant = False, memAliasGroup = Nothing }

constantMemory :: MemAttributes
constantMemory =
    MemAttributes { memConstant = True, memAliasGroup = Nothing }

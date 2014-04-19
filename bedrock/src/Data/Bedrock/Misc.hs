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
        Node         ->
            ( nsNextNodeId ns
            , incGlobal ns{nsNextNodeId = nsNextNodeId ns + 1})
        StaticNode{} ->
            ( nsNextNodeId ns
            , incGlobal ns{nsNextNodeId = nsNextNodeId ns + 1})
        Primitive    ->
            ( nsNextPrimitiveId ns
            , incGlobal ns{nsNextPrimitiveId = nsNextPrimitiveId ns + 1})
  where
    incGlobal thisNS = thisNS{ nsNextGlobalId = nsNextGlobalId thisNS + 1}

newGlobalID :: AvailableNamespace -> (Int, AvailableNamespace)
newGlobalID ns =
    ( nsNextGlobalId ns
    , ns{nsNextGlobalId = nsNextGlobalId ns + 1})

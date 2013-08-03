{-# LANGUAGE OverloadedStrings #-}
module Grin.Stage2.FromStage1
    ( convert
    ) where

import qualified Grin.Types as Stage1
import Grin.Stage2.Types as Stage2

import Grin.HPT

import Control.Monad.RWS
import qualified Data.Map as Map

import Config

convert :: OptConfig -> HeapAnalysis -> Stage1.Grin -> Stage2.Grin
convert optConfig hpt grin
    = let initReader = Env { envOptConfig    = optConfig
                           , envHeapAnalysis = hpt
                           , envNodeMap      = Map.empty }
          initState  = Stage1.grinUnique grin
          convertFuncs = do nodes <- funcDefsToNodes (Stage1.grinFunctions grin)
                            withNodeMap nodes $ do funcs <- mapM convertFuncDef (Stage1.grinFunctions grin)
                                                   cafs <- mapM convertCAF (Stage1.grinCAFs grin)
                                                   return (funcs, cafs, nodes)
      in case runRWS convertFuncs initReader initState of
           ((funcs, cafs, nodes), newUnique, stringCAFs)
             -> Grin { grinNodes     = Stage1.grinNodes grin ++
                                       [ NodeDef name FunctionNode []
                                       | names <- Map.elems nodes
                                       , name <- names ]
                     , grinCAFs      = cafs ++
                                       [ CAF { cafName = name, cafValue = Lit (Lstring string) }
                                         | (name, string) <- stringCAFs ]
                     , grinFunctions = funcs
                     , grinEntryPoint = Stage1.grinEntryPoint grin
                     , grinUnique    = newUnique
                     }

convertCAF :: Stage1.CAF -> M Stage2.CAF
convertCAF caf
    = do value <- case Stage1.cafValue caf of
                    Stage1.Node tag nt missing _args -> do tag' <- lookupTag tag missing
                                                           return $ Node tag' nt missing
                    other -> error $ "Grin.Stage2.FromStage1.convertCaf: Weird caf: " ++ show other
         return $ CAF { cafName  = Stage1.cafName caf
                      , cafValue = value }


type NodeMap = Map.Map Renamed [Renamed]
data Env = Env { envOptConfig    :: OptConfig
               , envHeapAnalysis :: HeapAnalysis
               , envNodeMap      :: NodeMap }
type M a = RWS Env [(Renamed,String)] Int a

funcDefToNode :: Stage1.FuncDef -> M (Renamed, [Renamed])
funcDefToNode def
    = do vars <- replicateM (arity+1) (newVariableFrom name)
         return (name, vars)
    where arity = length (Stage1.funcDefArgs def)
          name  = Stage1.funcDefName def

funcDefsToNodes :: [Stage1.FuncDef] -> M NodeMap
funcDefsToNodes defs
    = do nodes <- mapM funcDefToNode defs
         return $ Map.fromList nodes

withNodeMap :: NodeMap -> M a -> M a
withNodeMap nodes
    = local (\env -> env{ envNodeMap = Map.union nodes (envNodeMap env) })

convertFuncDef :: Stage1.FuncDef -> M Stage2.FuncDef
convertFuncDef def
    = do body <- convertExpression (Stage1.funcDefBody def)
         returns <- nodeSize (Stage1.funcDefName def)
         return $ FuncDef { funcDefName = Stage1.funcDefName def
                          , funcDefReturns = returns
                          , funcDefArgs = Stage1.funcDefArgs def
                          , funcDefBody = body
                          }

convertExpression :: Stage1.Expression -> M Stage2.Expression
convertExpression (Stage1.Application (Builtin "unreachable") [] Stage1.:>>= _)
    = return $ Application (Builtin "unreachable") []
convertExpression (a Stage1.:>>= v Stage1.:-> b)
    = do a' <- convertExpression a
         convertBind v $ \v' ->
           do b' <- convertExpression b
              return $ a' :>>= v' :-> b'
convertExpression (a Stage1.:>> b)
    = do a' <- convertExpression a
         b' <- convertExpression b
         return $ a' :>>= [] :-> b'
convertExpression (Stage1.Application (Builtin "fetch") [p])
    = convertFetch p
convertExpression (Stage1.Update 0 ptr val)
    = return $ Application (Builtin "unreachable") []
convertExpression (Stage1.Update size ptr val)
    = do [ptr'] <- lookupVariable ptr
         values <- fmap (take size) $ lookupVariable val
         minNodeSize <- asks (optSmallNodeSize . envOptConfig)
         if length values <= minNodeSize
            then return $ Application fn (ptr':values)
            else do extra <- newVariable
                    let (first,second) = splitAt (minNodeSize-1) values
                    return $ Store (length second) second :>>= [extra] :-> Application fn (ptr':first++[extra])
    where fn = Builtin "update"
convertExpression (Stage1.Application fn args)
    = do args' <- mapM lookupVariable args
         return $ Application fn (map head args')
convertExpression (Stage1.Case scrut alts)
    = do vector <- lookupVariable scrut
         case vector of
           [] -> return $ Application (Builtin "unreachable") []
           _  ->do alts'  <- mapM (convertAlt vector) alts
                   return $ Case (head vector) alts'
convertExpression (Stage1.Store _size (Stage1.Hole size))
    = return $ StoreHole size
convertExpression (Stage1.Store size val)
    = convertValue worker val
    where worker args
              = do minNodeSize <- asks (optSmallNodeSize . envOptConfig)
                   if size <= minNodeSize
                      then return (Store size args)
                      else do let (firstArgs, secondArgs) = splitAt (minNodeSize-1) args
                              extra <- newVariable
                              return $ Store (length secondArgs) secondArgs :>>= [extra] :-> Store minNodeSize (firstArgs++[extra])
convertExpression (Stage1.Unit val)
    = convertValue (return . Unit) val

convertBind :: Renamed -> ([Renamed] -> M a) -> M a
convertBind val fn
    = do size <- nodeSize val
         vars <- replicateM (max 1 size) newVariable
         local (\env -> env{ envNodeMap = Map.insert val vars (envNodeMap env)}) $ fn vars



{-
do node <- fetch p
   node `elem` [ Nil, Cons x y, NearBig x y z, Big x y z n ]
===>
do tag <- fetch 0 p
   d1 <- fetch 1 p
   d2 <- fetch 2 p
   d3 <- fetch 3 p
   [x,y] <- case tag of
              Nil  -> do unit []
              Cons -> do unit [d1,d2]
              NearBig -> do unit [d1,d2,d3]
              Big  -> do z <- fetch 0 d3
                         n <- fetch 1 d3
                         unit [d1,d2,z,n]
-}
convertFetch p
    = do rhs <- heapNodeValues p
         case rhs of
           Other{rhsTagged = nodes} | not (Map.null nodes)
             -> do let size = rhsSize rhs
                   [p'] <- lookupVariable p
                   tag <- newVariable
                   minNodeSize <- asks (optSmallNodeSize . envOptConfig)
                   prefetched <- replicateM (minNodeSize-1) newVariable
                   tmps <- replicateM (size-1) newVariable
                   alts <- mapM (mkAlt p' prefetched) (Map.toList nodes)
                   return $ fetchMany p' (tag:prefetched) $ Case tag alts :>>= tmps :-> Unit (tag:tmps)
                   -- Stage2.Fetch 0 p' :>>= [v] :-> Case v alts :>>= tmps :-> Unit (v:tmps)
           Base
             -> do [p'] <- lookupVariable p
                   return (Stage2.Fetch 0 p')
           Heap _
             -> do [p'] <- lookupVariable p
                   return (Stage2.Fetch 0 p')
           _ -> do return (Application (Builtin "unreachable") [])
    where mkAlt p prefetched ((tag, nt, missing), args)
              | length args <= length prefetched
              = do tag' <- lookupTag tag missing
                   return $ Node tag' nt missing :> Unit (take (length args) prefetched)
              | otherwise
              = do let extra = last prefetched
                       prefetchedData = init prefetched
                   argVars <- replicateM (length args - length prefetchedData) newVariable
                   let fetches = foldr (\(v,n) r -> Stage2.Fetch n extra :>>= [v] :-> r) (Unit (prefetchedData ++ argVars)) (zip argVars [0..])
                   tag' <- lookupTag tag missing
                   return $ Node tag' nt missing :> fetches

fetchMany ptr binds rest
    = worker 0 binds
    where worker n [] = rest
          worker n (x:xs) = Stage2.Fetch n ptr :>>= [x] :-> worker (n+1) xs

nodeSize :: Renamed -> M Int
nodeSize val
    = do hpt <- asks envHeapAnalysis
         return (rhsSize (lookupLhs (VarEntry val) hpt))

heapNodeValues :: Renamed -> M Rhs
heapNodeValues val
    = do hpt <- asks envHeapAnalysis
         return (lookupHeap val hpt)

newVariable :: M Renamed
newVariable = newVariableFrom (Builtin "newVariable.undefined")

newVariableFrom :: Renamed -> M Renamed
newVariableFrom original
    = do u <- get
         put (u+1)
         return $ merge original $ Anonymous u
    where merge (Aliased _ name) (Anonymous uid) = Aliased uid name
          merge _ renamed = renamed

convertAlt :: [Renamed] -> Stage1.Alt -> M Stage2.Alt
convertAlt vector (Stage1.Lit lit Stage1.:> alt)
    = do alt' <- convertExpression alt
         return $ Lit lit :> alt'
convertAlt vector (Stage1.Variable v Stage1.:> alt)
    = convertBind v $ \v' ->
      do alt' <- convertExpression alt
         return $ Stage2.Empty :> Unit vector :>>= v' :-> alt'
convertAlt vector (Stage1.Node tag FunctionNode missing args Stage1.:> alt)
    = do names <- lookupVariable tag
         alt' <- convertExpression alt
         return $ Node (names !! missing) FunctionNode missing :> Unit (tail vector) :>>= args :-> alt'
convertAlt vector (Stage1.Node tag nt missing args Stage1.:> alt)
    = do alt' <- convertExpression alt
         return $ Node tag nt missing :> Unit (tail vector) :>>= args :-> alt'
convertAlt vector (Stage1.Vector args Stage1.:> alt)
    = do alt' <- convertExpression alt
         return $ Stage2.Empty :> Unit vector :>>= args :-> alt'
convertAlt vector (Stage1.Empty Stage1.:> alt)
    = error $ "Grin.Stage2.FromStage1.convertAlt: Empty case condition."
convertAlt vector (Stage1.Hole{} Stage1.:> alt)
    = error $ "Grin.Stage2.FromStage1.convertAlt: Invalid case condition."

convertValue :: ([Renamed] -> M Expression) -> Stage1.Value -> M Expression
convertValue fn (Stage1.Lit (Lstring string))
    = do v <- newVariable
         tell [(v,string)]
         fn [v]
convertValue fn (Stage1.Lit lit)    = do v <- newVariable
                                         r <- fn [v]
                                         return $ Constant (Lit lit) :>>= [v] :-> r
convertValue fn Stage1.Hole{}       = error "Grin.Stage2.FromStage1.convertValue: There shouldn't be a hole here."
convertValue fn (Stage1.Empty)      = fn []
convertValue fn (Stage1.Variable v) = fn =<< lookupVariable v
convertValue fn (Stage1.Node tag nt missing args)
    = do tag' <- lookupTag tag missing
         v <- newVariable
         args' <- mapM lookupVariable args
         r <- fn (v:concat args')
         return $ Constant (Node tag' nt missing) :>>= [v] :-> r
convertValue fn (Stage1.Vector args)
    = do args' <- mapM lookupVariable args
         fn (concat args')

lookupVariable :: Renamed -> M [Renamed]
lookupVariable val
    = do nmap <- asks envNodeMap
         return $ Map.findWithDefault [val] val nmap

lookupTag :: Renamed -> Int -> M Renamed
lookupTag tag idx
    = do tags <- lookupVariable tag
         return (cycle tags !! idx)
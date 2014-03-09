
module Grapher where

import Data.Graph
import qualified Data.Map as M
import H.Common

import Monad
import Resource
import Value

type HeapGraph = M.Map Ptr (Resource, Ptr, [Ptr])

type GraphM a = StateT HeapGraph M a

graphPhase :: (Ptr, Heap) -> M [Resource]
graphPhase (main, heap) =
  checked
   $  flattenSCCs
   .  stronglyConnComp
   .  M.elems
  <$> execGraph (heapGraph heap main)

execGraph :: GraphM () -> M HeapGraph
execGraph = flip execStateT M.empty

heapGraph :: Heap -> Ptr -> GraphM ()
heapGraph heap = f
  where
    f ptr = gets (M.member ptr) >>= \case
      True  -> return ()
      False -> mapM_ f deps >> modify (M.insert ptr (res, ptr, deps))
      where
        Rec{..} = maybe (impossible "Missing pointer in heap") id (lookup ptr heap)
        (deps, attrs) = partitionEithers $ map g recBody
        g (name, val) = case val of
          VRec ptr -> Left ptr
          VLit lit -> Right $ Just (name, lit)
          VFun _ _ -> Right Nothing
          VPrim _  -> Right Nothing
        res =
          Resource
          { _rName  = todo
          , _rType  = recType
          , _rAttrs = M.fromList . catMaybes $ attrs
          }


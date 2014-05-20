{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module MarkovChain where

import Data.Monoid
import Data.List (foldl')
import Data.Graph.Inductive

type MarkovChain a = Gr (a, Int) Int

instance Monoid (MarkovChain a) where
  mempty  = empty
  mappend = addChains

addChains mc1 mc2 = foldr (&) mc1 newContexts
  where oldContexts = map (context mc2) (nodes mc2)
        newContexts = zipWith changeNodeNumber oldContexts openNodeNumbers
        changeNodeNumber (a,_,b,c) x = (a, x, b, c)
        openNodeNumbers = newNodes (length oldContexts) mc1

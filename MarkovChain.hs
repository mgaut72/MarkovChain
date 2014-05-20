{-# LANGUAGE TypeSynonymInstances #-} -- for Monoid instance
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-} -- for custom "Show"
module MarkovChain where

import Data.Monoid
import Data.List
import Data.Maybe
import Data.Graph.Inductive
import qualified Data.Map as Map

type MarkovChain a = Gr (a) Int

instance Monoid (MarkovChain a) where
  mempty  = empty
  mappend = addChains

instance (Show a) => Show (MarkovChain a) where
  show = init . prettify

addChains mc1 mc2 = foldr (&) mc1 newContexts
  where oldContexts = map (context mc2) (nodes mc2)
        newContexts = zipWith changeNodeNumber oldContexts openNodeNumbers
        changeNodeNumber (a,_,b,c) x = (a, x, b, c)
        openNodeNumbers = newNodes (length oldContexts) mc1

markovChain :: (Eq a) => [a] -> MarkovChain a
markovChain xs = mkGraph ns es
 where ns = makeNodes xs
       es = makeEdges ns xs

makeNodes :: (Eq a) => [a] -> [LNode a]
makeNodes xs = zip [0..] (nub xs)

makeEdges ns xs = map flatten (Map.toList m)
 where flatten ((a,b),c) = (a,b,c)
       m = makeEdgesH ns xs

makeEdgesH :: (Eq a) => [LNode a] -> [a] -> Map.Map (Node, Node) Int
makeEdgesH ns [_]      = Map.empty
makeEdgesH ns (x:y:ys) = Map.insertWith' (+) (n1,n2) 1 m
 where m = makeEdgesH ns (y:ys)
       n1 = lookupNode ns x
       n2 = lookupNode ns y

lookupNode nodes label = n
 where (n,_) = fromJust $ find (\(_,l) -> l == label) nodes

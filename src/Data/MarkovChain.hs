{-# LANGUAGE TypeSynonymInstances #-} -- for Monoid instance
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-} -- for custom "Show"
module Data.MarkovChain
  (
    markovChain     -- Constructor
  , traverse        -- generate a random traversal of the chain
  ) where

import Data.Monoid
import Data.List
import Control.Monad.State
import Data.Maybe
import Data.Graph.Inductive
import System.Random
import Data.Random
import Data.Random.Source
import Data.Random.Extras
import qualified Data.Map as Map

type MarkovChain a = Gr (a) Int

instance Monoid (MarkovChain a) where
  mempty  = empty
  mappend = addChains

instance (Show a) => Show (MarkovChain a) where
  show mc = init . concat $ map showContext $ gsel (const True) mc
    where showContext (_,_,nodeLabel,links) = show nodeLabel
                                           <> "->"
                                           <> (show $ llnk links)
                                           <> "\n"
          llnk = map (\(weight, n) -> (fromJust $ lab mc n, weight))


--
-- type constructor
--
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

--
-- Random traversal of the data structure
--
traverse :: StdGen -> MarkovChain a -> Int -> [a]
traverse g mc n = fst $ traverseH g' mc n start
  where (start, g') = rChoice g $ nodes mc

traverseH :: StdGen -> MarkovChain a -> Int -> Int -> ([a], StdGen)
traverseH g mc  0 node = ([a], g)
  where a = fromJust $ lab mc node
traverseH g mc n node = (a:as, g'')
  where (as, g'') = traverseH g' mc (n-1) nextNode
        a = fromJust $ lab mc node
        (nextNode, g') = rChoice g connected
        connected = concatMap (\(_,n,w) -> replicate w n) $ out mc node

rChoice :: StdGen -> [a] -> (a, StdGen)
rChoice g xs = runState (runRVar (choice xs) StdRandom) g


--
-- for the monoid instance
--
addChains mc1 mc2 = foldr (&) mc1 newContexts
  where oldContexts = map (context mc2) (nodes mc2)
        newContexts = zipWith changeNodeNumber oldContexts openNodeNumbers
        changeNodeNumber (a,_,b,c) x = (a, x, b, c)
        openNodeNumbers = newNodes (length oldContexts) mc1



{-# LANGUAGE TypeSynonymInstances #-} -- for Monoid instance
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-} -- for custom "Show"
module Data.MarkovChain
  (
    MarkovChain     -- Type
  , markovChain     -- Constructor
  , traverse        -- generate a random traversal of the chain
  , traverse'       -- generate a random traversal of the chain, constant seed
  ) where

import Data.Monoid
import Control.Monad.State
import Data.Maybe
import System.Random
import Data.Random
import Data.Random.Source
import Data.Random.Extras
import Data.Sequence
import qualified Data.Map as Map

-- keep the implementation hidden
type MarkovChain a = MarkovChainImpl a
type MarkovChainImpl a = Map.Map a (Seq a)

instance Ord a => Monoid (MarkovChain a) where
  mempty  = Map.empty
  mappend = Map.unionWith mappend

--instance (Show a) => Show (MarkovChain a) where
--  show mc = undefined

--
-- type constructor
--
markovChain :: (Ord a) => [a] -> MarkovChain a
markovChain []     = Map.empty
markovChain [x]    = Map.singleton x empty
markovChain (x:y:xs) = Map.insertWith' mappend x (singleton y) (markovChain (y:xs))

--
-- Random traversal of the data structure
--
traverse' :: Ord a => MarkovChain a -> Int -> [a]
traverse' = traverse $ mkStdGen 75812369417

traverse :: Ord a => StdGen -> MarkovChain a -> Int -> [a]
traverse g mc n = fst $ traverseH g' mc n start
  where (start, g')  = rChoice g $ Map.keys mc
        rChoice g xs = runState (runRVar (choice xs) StdRandom) g

traverseH :: Ord a => StdGen -> MarkovChain a -> Int -> a -> ([a], StdGen)
traverseH g mc _ a | Map.lookup a mc == Just empty = ([a], g)
traverseH g mc 0 a = ([a], g)
traverseH g mc n a = (a:as, g'')
  where (as, g'')  = traverseH g' mc (n-1) nxtA
        (nxtA, g') = rChoice g $ fromJust $ Map.lookup a mc
        rChoice g xs = runState (runRVar (choiceSeq xs) StdRandom) g

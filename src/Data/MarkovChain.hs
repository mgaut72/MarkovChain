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
import System.Random
import Data.Random
import Data.Random.Source
import Data.Random.Extras
import qualified Data.Map as Map
import qualified Data.MultiSet as MSet

type MarkovChain a = Map.Map a (MSet.MultiSet a)

instance Ord a => Monoid (MarkovChain a) where
  mempty  = Map.empty
  mappend = Map.unionWith MSet.union

--instance (Show a) => Show (MarkovChain a) where
--  show mc = undefined

--
-- type constructor
--
markovChain :: (Ord a) => [a] -> MarkovChain a
markovChain []     = Map.empty
markovChain [x]    = Map.singleton x MSet.empty
markovChain (x:y:xs) = Map.insertWith' MSet.union x (MSet.singleton y) (markovChain (y:xs))

--
-- Random traversal of the data structure
--
traverse :: Ord a => StdGen -> MarkovChain a -> Int -> [a]
traverse g mc n = fst $ traverseH g' mc n start
  where (start, g') = rChoice g $ Map.keys mc

traverseH :: Ord a => StdGen -> MarkovChain a -> Int -> a -> ([a], StdGen)
traverseH g mc 0 a = ([a], g)
traverseH g mc n a = (a:as, g'')
  where (as, g'') = traverseH g' mc (n-1) nextA
        potential = MSet.toList . fromJust $ Map.lookup a mc
        (idx, g') = randomR (0, length potential) g
        nextA     = potential !! idx

rChoice :: StdGen -> [a] -> (a, StdGen)
rChoice g xs = runState (runRVar (choice xs) StdRandom) g

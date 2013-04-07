{-# LANGUAGE FlexibleContexts #-}

import System.Random (StdGen, mkStdGen, randomR, newStdGen, randoms, random)
import qualified Data.Map as M hiding(map)
import Control.Monad.State

gen :: StdGen
gen = mkStdGen 1000

languages :: [String]
languages =
  ["python"
  ,"haskell"
  ,"ruby"
  ,"c"
  ,"cpp"
  ,"javascript"
  ,"php"
  ,"go"
  ,"elisp"
  ,"scheme"
  ,"clojure"
  ,"erlang"
  ]

randomSamples source gen = map ((!!) source) indexes
  where len = length source
        indexes = map (flip mod len) (randoms gen)

newtype Counts k v = Counts [(k, v)] deriving Show
type CountState = State (Counts String Int)

addCount :: (Num t, Ord k) => k -> Counts k t -> (t, Counts k t)
addCount key (Counts m) =  (result, newMem) where
  defaultVal = 1
  counts = M.fromList m
  storedVal = M.lookup key $ counts
  (result, newMem) = case storedVal of
    Nothing -> (defaultVal, Counts ((key, defaultVal):m))
    Just x -> (x + 1, Counts (M.toList $ M.adjust (+ 1) key counts))

baseCount :: Counts k v
baseCount = Counts []

addCountSt :: (Num a, Ord k, MonadState (Counts k a) m) => k -> m a
addCountSt key = state $ (addCount key)

randomLanguages :: [String]
randomLanguages = take 50 $ randomSamples languages gen

languageFrequencies :: [(String, Integer)]
languageFrequencies = getList counts
  where counts = snd $ runState (mapM addCountSt randomLanguages) baseCount
        getList = \(Counts xs) -> xs

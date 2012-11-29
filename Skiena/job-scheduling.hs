-- 1.2 Selecting the right job (Skeina)

import System.Random
import Control.Monad.State
import Debug.Trace
import Data.List
import Data.Maybe (fromJust)

jobRange :: (Int, Int)
jobRange = (1, 30)

-- newtype Job a = Job (a, a) deriving Show
newtype Job a = Job (a, a)
-- Print job nicely over a timeline (probably a time waste)
instance (Show a, Integral a) => Show (Job a) where
  show (Job (start, end)) = "Job: " ++ (show (start, end)) ++ "\t|" ++ head ++ range ++ tail ++ "|\n" where
    head = interval (start - fromIntegral(fst jobRange)) ' '
    range = interval (end - start) '_'
    tail = interval (fromIntegral(snd jobRange) - end) ' '

    interval i char = (replicate (getCols $ fromIntegral i) char)
    getCols size = round $ colRatio * size
    colRatio =  60 / toRational (snd jobRange)

instance (Eq a) => Eq (Job a) where
  (==) (Job (s1, e1)) (Job (s2, e2)) = s1 == s2 && e1 == e2

randomSt :: (RandomGen g, Random a) => (a, a) -> State g a
randomSt range = state (randomR range)

randomPair :: (RandomGen g, Random a) => (a, a) -> State g (a, a)
randomPair range = do
  start <- randomSt range
  end <- randomSt range
  return $ (start, end)

randomPairs :: (RandomGen g, Random a) => Int -> (a, a) -> State g [(a, a)]
randomPairs count range = mapM (\_ -> (randomPair range)) [1..count]

mkJob :: (Ord a) => (a, a) -> Maybe (Job a)
mkJob (start, end)
  | start > end = Just(Job (end, start))
  | start < end = Just(Job (start, end))
  | otherwise = Nothing

randomJobs count seed = map fromJust $ filter filterJust $ map mkJob pairs where
  pairs = fst $ runState (randomPairs count jobRange) (mkStdGen seed)
  filterJust Nothing = False
  filterJust  _ = True

-- baseFilter: Filters jobs impossible based on the problem statement
baseFilter :: (Ord a) => Maybe (Job a) -> [Job a] -> [Job a]
baseFilter (Just (Job (s, e))) = filter (\(Job (s', e')) -> s' > e)
baseFilter Nothing = id

----------------
-- Job selectors 
----------------
startSort, endSort :: (Ord a) => Job a -> Job a -> Ordering
startSort j1@(Job (s1, e1)) j2@(Job (s2, e2))
  | s1 < s2 = LT
  | s1 == s2 && e1 < e2 = LT
  | j1 == j2 = EQ
  | otherwise = GT

endSort j1@(Job (s1, e1)) j2@(Job (s2, e2))
  | e1 < e2 = LT
  | e1 == e2 && s1 > s2 = LT -- Look for jobs starting latest (I wanna work for the least duration :P)
  | j1 == j2 = EQ
  | otherwise = GT


validSel :: (Ord a) => [Job a] -> Maybe [Job a]
validSel = (foldr foldingFunction (Just [])).sortBy startSort where
  -- Works only with foldr
  foldingFunction _ Nothing = Nothing
  foldingFunction j (Just []) = Just [j]
  foldingFunction j@(Job (s', e')) acc@(Just (Job (s, e):_))
                   | s >= e' = fmap (j:) acc
                   | otherwise = Nothing

-- Becomes unusable around early 20s
exhaustiveSel :: (Ord a) => [Job a] -> [Job a]
exhaustiveSel = head.(sortBy lengthSort).validSubsets.sortBy startSort where
  lengthSort set1 set2 = compare (length set2) (length set1)
  validSubset selection = case (validSel selection) of
    Nothing  -> False
    (Just _) -> True
  validSubsets jobs = filter validSubset $ filterM (\_ -> [True, False]) jobs

-- endFirstSel: Selector which prefers jobs ending first (optimal)
endFirstSel :: (Ord a) => [Job a] -> [Job a]
endFirstSel = endFirstFilter.sortBy endSort where
  endFirstFilter (current:jobs') = current:(endFirstFilter $ baseFilter (Just current) jobs')
  endFirstFilter [] = []

jobSample = randomJobs 15 10

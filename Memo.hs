import Data.Map as M hiding(map)
import Debug.Trace
import Data.Maybe (fromJust)
import Control.Monad.State
-- import System.Random

memo :: (Int -> a) -> Int -> a
memo f = let cache = M.fromList $ map (\a -> (a, f a)) [-100..100]
         in (\x -> fromJust $ M.lookup x cache)
 
bomb :: (Num a, Show a) => a -> a
bomb x = x + 2

bombT x = trace (show x) bomb x

bombM :: Int -> Int
bombM = memo bombT

---------------------------------
-- State monad based memorizer --
---------------------------------

newtype MemoSt k v = MemoSt [(k, v)] deriving Show

memoizer :: Ord k => (k -> v) -> k -> MemoSt k v   -> (v, MemoSt k v)
memoizer f k (MemoSt m) =  (result, newMem) where
  calculatedVal = f k
  storedVal = M.lookup k $ M.fromList m
  (result, newMem) = case storedVal of
    Nothing -> (calculatedVal, MemoSt ((k, calculatedVal):m))
    Just x -> (x, MemoSt m)

memoize :: (Ord k) => (k -> v) -> k -> State (MemoSt k v) v
memoize f k = state $ \m -> memoizer f k m

bombS, bombS' :: Int -> State (MemoSt Int Int) Int
bombS k = memoize bomb k
bombS' k = state $ \m -> memoizer bomb k m

bombMapS = MemoSt $ map (\a -> (a, bomb a)) [1..10] :: MemoSt Int Int

bombTestS :: State (MemoSt Int Int) Int 
bombTestS = do
  bombS 2
  bombS' 3
  bombS' 45
  bombS 20

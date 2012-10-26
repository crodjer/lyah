module Learn.Monad where

import Data.Monoid

newtype Logger w a = Logger {runLogger :: (a, w)}

instance (Monoid w) =>  Monad (Logger w) where
    return x = Logger (x, mempty)
    Logger (a, l) >>= f = Logger (b, l `mappend` l') where
        (b, l') = runLogger $ f a


calcLogEntry :: (Show a) => a -> a -> String -> a -> String
calcLogEntry x y o r = show x ++ " " ++ o ++  " " ++ show y ++ " = " ++ show r

loggedAdd :: (Num a, Show a) => a -> a -> Logger [String] a
loggedAdd x y = Logger (r, [calcLogEntry y x "+" r]) where
                    r = y + x

loggedMul :: (Num a, Show a) => a -> a -> Logger [String] a
loggedMul x y = Logger (r, [calcLogEntry y x "*" r]) where
                    r = y * x

loggedSub :: (Num a, Show a) => a -> a -> Logger [String] a
loggedSub x y = Logger (r, [calcLogEntry y x "-" r]) where
                    r = y - x

loggedDiv :: (Fractional a, Show a) => a -> a -> Logger [String] a
loggedDiv x y = Logger (r, [calcLogEntry y x "/" r]) where
                    r = y / x

loggedSqr :: (Num a, Show a) => a -> Logger [String] a
loggedSqr a = loggedMul a a

test :: (Fractional a, Show a) => Logger [String] a
test = loggedAdd 20 0
       >>= loggedAdd 20
       >>= loggedSub 5
       >>= loggedMul 3
       >>= loggedDiv 5
       >>= loggedSqr

showLogger :: (Show a) => Logger [String] a -> IO ()
showLogger logger = do
  (result, log) <- return $ runLogger $ logger
  putStrLn  $ "Output: " ++ show result
  putStrLn "Log: "
  putStr $ foldr (\f s -> "\t" ++ f ++ "\n" ++ s) "" log

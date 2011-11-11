import System.Environment
import Data.List

main = do
    args <- getArgs
    progName <- getProgName
    putStr "The arguments are:"
    mapM putStrLn args
    putStrLn "The program name is:"
    putStrLn progName

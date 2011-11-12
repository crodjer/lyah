import System.Environment
import System.IO.Error

main = toTry `catch` handler

toTry :: IO ()

toTry = do
    (fileName:_) <- getArgs
    contents <- readFile fileName
    putStrLn $ "The file has " ++ show (length(lines contents)) ++ " lines!"

handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e =
        case ioeGetFileName e of
            Just path -> putStrLn $ "Whoops! File does not exist at: " ++ path
            Nothing -> putStrLn "Whoops! File does not exist at unknown location!"

    | isFullError e = putStrLn "Not enough space! please free some."
    | isIllegalOperation e = putStrLn "Thats Illegal!"
    | isUserError e = putStrLn "Please provide a file name!"
    | otherwise = ioError e

{-
 -main = do
 -    (fileName:_) <- getArgs
 -    fileExists <- doesFileExist fileName
 -    if fileExists
 -        then do
 -            contents <- readFile fileName
 -            putStrLn $ "The file has " ++ show (length(lines contents)) ++ " lines!"
 -        else do
 -            putStrLn "The file doesn't exist!"
 -}

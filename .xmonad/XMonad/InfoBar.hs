module XMonad.InfoBar where

import System.IO

updateInfoBar :: String -> IO ()
updateInfoBar i = do
    h <- openFile "/home/jbarratt/.info" WriteMode
    hPutStrLn h i
    hClose h

readInfoBar :: IO String
readInfoBar = do
    h <- openFile "/home/jbarratt/.info" ReadMode
    r <- hGetContents h
    r `seq` hClose h
    return r

{-# LANGUAGE OverloadedStrings #-}

module XMonad.Task where

import Control.Applicative
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List
import Data.Maybe
import XMonad.Util.Run
import XMonad.InfoBar

data Task = Task    { taskId :: Int
                    , taskDesc :: String
                    , taskStart :: Maybe String
                    }

instance FromJSON Task where
    parseJSON (Object v) = Task <$>
        v .: "id" <*>
        v .: "description" <*>
        v .:? "start"

instance Show Task where
    show t = taskDesc t ++ " (" ++ show (taskId t) ++ ")"

getTopTasks :: IO [String]
getTopTasks = do
    rawText <- runProcessWithInput
        "task"
        ["due.before:tomorrow", "export"]
        ""
    let rawList = "[" ++ rawText ++ "]" :: String
    case decode $ BS.pack rawList of
        Just ts -> return . fmap show . filter isActive $ ts
        Nothing -> return []

getCurrentTasks :: IO [String]
getCurrentTasks = do
    rawText <- runProcessWithInput "task" ["export"] ""
    let rawList = "[" ++ rawText ++ "]" :: String
    case decode $ BS.pack rawList of
        Just ts -> return $ filterCurrent ts
        Nothing -> return []
    where
        filterCurrent :: [Task] -> [String]
        filterCurrent = fmap show
            . filter isStarted
            . filter isActive

isActive :: Task -> Bool
isActive = (/= 0) . taskId

isStarted :: Task -> Bool
isStarted = isJust . taskStart

getTask :: Int -> IO String
getTask n = do
    tasks <- getTopTasks
    case drop n tasks of
        [] -> return "No task"
        (t:_) -> return $ "Task " ++ show n ++ ": " ++ t

showTask :: Int -> IO ()
showTask n = getTask n >>= updateInfoBar

showTasks :: IO ()
showTasks = do
    tasks <- getCurrentTasks
    updateInfoBar $ formatTasks tasks
    where
        formatTasks :: [String] -> String
        formatTasks = ("Tasks: " ++)
            . intercalate " | "
            . reverse
            . fmap formatTask
            . zip [0..]
        formatTask :: (Int, String) -> String
        formatTask (i, d) = show i ++ " - " ++ d

getCurrentTask :: IO (Maybe Int)
getCurrentTask = do
    i <- readInfoBar
    return $ findNextTask i
    where
        findNextTask s =
            case words s of
                ("Task":ns:_) ->
                    case reads ns of
                        ((n,_):_) -> Just n
                        _ -> Nothing
                _ -> Nothing

getNextTask :: IO Int
getNextTask = do
    ct <- getCurrentTask
    return $ case ct of
        Just n -> n + 1
        Nothing -> 0

getPreviousTask :: IO Int
getPreviousTask = do
    ct <- getCurrentTask
    return $ case ct of
        Just n -> n - 1
        Nothing -> 0

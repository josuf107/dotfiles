-- xmonad config used by Vic Fryzel
-- Author: Vic Fryzel
-- http://github.com/vicfryzel/xmonad-config

{-# LANGUAGE OverloadedStrings #-}
 
import Control.Applicative
import Control.Exception
import Control.Monad
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List
import Data.Maybe
import System.IO
import System.Exit
import System.Process
import System.Random
import System.Directory
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.NoBorders
import XMonad.Layout.Circle
import XMonad.Layout.Tabbed
import XMonad.Layout.Fullscreen
import XMonad.Layout.PerWorkspace
import XMonad.Prompt
import XMonad.Prompt.AppendFile
import XMonad.Prompt.Shell
import XMonad.Util.Dmenu
import XMonad.Util.Run
import XMonad.Util.EZConfig(additionalKeys)

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
 
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal :: String
{-myTerminal      = "gnome-terminal --hide-menubar"-}
myTerminal      = "urxvt"

terminalRun :: MonadIO m => String -> m ()
terminalRun c = spawn $ myTerminal ++ " -e " ++ c
 
-- Width of the window border in pixels.
--
myBorderWidth :: Dimension
myBorderWidth   = 1
 
-- System font
--
myFont :: String
myFont 					= "FreeMono"

promptConfig :: XPConfig
promptConfig = 
    defaultXPConfig {
        font = "xft:inconsolata:size=16:antialias=true",
        bgColor = "#002b36",
        fgColor = "#839496",
        bgHLight = "#073642",
        fgHLight = "#cb4b16",
        borderColor = "#002b36",
        promptBorderWidth = 0,
        height = 25,
        position = Bottom
    }

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask
 
-- The mask for the numlock key. Numlock status is "masked" from the
-- current modifier status, so the keybindings will work with numlock on or
-- off. You may need to change this on some systems.
--
-- You can find the numlock modifier by running "xmodmap" and looking for a
-- modifier with Num_Lock bound to it:
--
-- > $ xmodmap | grep Num
-- > mod2        Num_Lock (0x4d)
--
-- Set numlockMask = 0 if you don't have a numlock key, or want to treat
-- numlock status separately.
--
myNumlockMask   = mod2Mask
 
-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = ["1:web","2:code","3:chat","4:media","5","6","7:writing","8","9","xmonad","network","task","bible"]

contexts = [ ("xmonad", xmonadContext)
           , ("network", networkContext)
           , ("task", taskContext)
           , ("poweroff", spawn "systemctl poweroff")
           , ("reboot", spawn "systemctl reboot")
           , ("code", windows (W.greedyView "2:code") >> spawn myTerminal >> spawn myTerminal)
           , ("bible", bibleContext)
           ]

bibleContext = do
    windows $ W.greedyView "bible"
    terminalRun "ghci /home/joseph07/projects/esv/study.hs"
    terminalRun "vim /home/joseph07/projects/esv/reading-plan.txt"

xmonadContext = do
    windows (W.greedyView "xmonad")
    terminalRun "vim /home/jbarratt/.xmonad/xmonad.hs"

networkContext = do
    windows (W.greedyView "network")
    terminalRun "wicd-curses"

taskContext = do
    windows (W.greedyView "task")
    terminalRun "task shell"
 
-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#002b36"
myFocusedBorderColor = "#268bd2"

dmenuArgs :: String -> [String]
dmenuArgs p =
    [ "-nb", "#002b36"
    , "-nf", "#839496"
    , "-sb", "#073642"
    , "-sf", "#cb4b16"
    , "-fn", "Inconsolata-16"
    , "-p", p
    ]

enquote :: String -> String
enquote q = "\"" ++ q ++ "\""

runDmenu :: MonadIO m => m ()
runDmenu = 
    let 
        args = unwords . fmap enquote . dmenuArgs $ "RUN:"
        dmenuCmd = "dmenu_run " ++ args
    in spawn dmenuCmd

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modMask}) = 
    let
        mapKey :: KeySym -> X () -> ((KeyMask, KeySym), X())
        mapKey k a = ((modMask, k), a)
        mapShiftKey :: KeySym -> X () -> ((KeyMask, KeySym), X())
        mapShiftKey k a = ((modMask .|. shiftMask, k), a)
    in
        M.fromList $
        -- launch a terminal
        [ mapShiftKey xK_Return $ spawn $ XMonad.terminal conf
        , mapShiftKey xK_l $ spawn "gnome-screensaver-command --lock"
        , mapShiftKey xK_m $ spawn "setxkbmap us && xmodmap ~/.caps_unswap"
        , mapShiftKey xK_d $ spawn "setxkbmap dvorak && xmodmap ~/.caps_swap"
        -- play/pause cmus
        , mapKey xK_apostrophe $ spawn "cmus-remote -u"
        -- launch dmenu
        , mapKey xK_p $ shellPrompt promptConfig
        , mapKey xK_b $ io getRandomBackground >>= spawn . ("feh --bg-scale " ++)
        -- close focused window 
        , mapKey xK_c $ kill
        -- Rotate through the available layout algorithms
        , mapKey xK_space $ sendMessage NextLayout
        --  Reset the layouts on the current workspace to default
        , mapShiftKey xK_space $ setLayout $ XMonad.layoutHook conf
        -- Resize viewed windows to the correct size
        , mapKey xK_n $ refresh
        -- Move focus to the next window
        , mapKey xK_Tab $ windows W.focusDown
        -- Move focus to the next window
        , mapKey xK_j $ windows W.focusDown
        -- Move focus to the previous window
        , mapKey xK_k $ windows W.focusUp
        -- Move focus to the previous window
        , mapShiftKey xK_Tab $ windows W.focusUp
        -- Move focus to the master window
        , mapKey xK_m $ windows W.focusMaster
        -- Swap the focused window and the master window
        , mapKey xK_Return $ windows W.swapMaster
        -- Swap the focused window with the next window
        , mapShiftKey xK_j $ windows W.swapDown
        -- Swap the focused window with the previous window
        , mapShiftKey xK_k $ windows W.swapUp
        -- Shrink the master area
        , mapKey xK_h $ sendMessage Shrink
        -- Expand the master area
        , mapKey xK_l $ sendMessage Expand
        -- Push window back into tiling
        , mapKey xK_t $ withFocused $ windows . W.sink
        -- Increment the number of windows in the master area
        , mapKey xK_comma $ sendMessage (IncMasterN 1)
        -- Deincrement the number of windows in the master area
        , mapKey xK_period $ sendMessage (IncMasterN (-1))
        -- Control volume
        , mapKey xK_bracketright $ spawn "amixer -q set Master 3+ unmute"
        , mapKey xK_bracketleft $ spawn "amixer -q set Master 3- unmute"
        , mapKey xK_backslash $ spawn "amixer -q set Master toggle"
        , mapKey xK_s $ appendFilePrompt promptConfig "/home/jbarratt/.scratch"
        , mapKey xK_i $ io (getNextTask >>= showTask)
        , mapShiftKey xK_i $ io (getPreviousTask >>= showTask)
        , mapKey xK_a $ io showTasks
        -- Quit xmonad
        , mapShiftKey xK_q $ io exitSuccess
        -- Restart xmonad
        , mapKey xK_q $ restart "xmonad" True
        , mapKey xK_0 $ contextualize
        , mapShiftKey xK_t $ shellPrompt promptConfig
        ]
        ++

        --
        -- mod-[1..9], Switch to workspace N
        -- mod-shift-[1..9], Move client to workspace N
        --
        [((m .|. modMask, k), windows $ f i)
            | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
            , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
            ++

        --
        -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
        -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
        --
        [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
            | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
            , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

contextualize :: X ()
contextualize = do
    s <- getSelection (fmap fst contexts)
    let gs = takeWhile (/='\n') s
    case lookup gs contexts of
        Just c -> c
        _ -> return ()


getSelection :: [String] -> X String
getSelection = menuArgs "dmenu" (dmenuArgs "CONTEXT:")
 
 
------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList
 
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), \w -> focus w >> mouseMoveWindow w)
 
    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), \w -> focus w >> windows W.swapMaster)
 
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), \w -> focus w >> mouseResizeWindow w)
 
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]
 
------------------------------------------------------------------------
-- Layouts:
 
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myTabConfig = defaultTheme {   activeBorderColor = "#7C7C7C"
                             , activeTextColor = "#CEFFAC"
                             , activeColor = "#000000"
                             , inactiveBorderColor = "#7C7C7C"
                             , inactiveTextColor = "#EEEEEE"
                             , inactiveColor = "#000000" }

myLayout = 	onWorkspace "2:code" (avoidStruts (tiled ||| Full)) $
            onWorkspace "3:chat" (avoidStruts (tiled ||| Full)) $
            onWorkspace "1:web" (avoidStruts tiled ||| fullscreenFull Full) $
            onWorkspace "4:media" (avoidStruts tiled ||| fullscreenFull Full) $
            onWorkspace "7:writing" (fullscreenFull Full) $
            avoidStruts (tiled ||| tabbed shrinkText myTabConfig ||| Circle) ||| fullscreenFull Full
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
 
     -- The default number of windows in the master pane
     nmaster = 1
 
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
 
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100
 
------------------------------------------------------------------------
-- Window rules:
 
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ manageDocks
    , className =? "Google-chrome"  --> doShift "1:web"
    , className =? "Firefox"  			--> doShift "1:web"
    , className =? "Racquet"  			--> doShift "6"
    , className =? "Pidgin"         --> doShift "3:chat"
    , className =? "Contact List"         --> doShift "3:chat"
    , className =? "Rhythmbox"         --> doShift "4:media"
    , className =? "Deadbeef"      --> doShift "4:media" ]
 
-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False
 
 
------------------------------------------------------------------------
-- Status bars and logging
 
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--
 
------------------------------------------------------------------------
-- Startup hook
 
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
    spawn "xmodmap ~/.caps_swap"
    io getRandomBackground >>= spawn . ("feh --bg-scale " ++)
    setWMName "LG3D"
    spawn "xrandr --output VGA1 --auto --left-of HDMI1"
    {-spawn "firefox"-}
 
getRandomBackground :: IO FilePath
getRandomBackground = do
    home <- getHomeDirectory
    setCurrentDirectory $ home ++ "/Pictures/backgrounds"
    old <- liftM extractbg . readFile $ home ++ "/.fehbg"
    bgs <- liftM (filter (\v -> length v > 2)) $ getDirectoryContents "." 
    (i,_) <- liftM (randomR (0, length bgs - 1)) newStdGen 
    new <- canonicalizePath . head . drop i $ bgs
    getHomeDirectory >>= setCurrentDirectory
    if new == old && length bgs > 1 then
        getRandomBackground
    else 
        return new
    where 
        extractbg = filter (/='\'') . head . drop 2 . words

data Task = Task { taskId :: Int, taskDesc :: String, taskStart :: Maybe String}

instance FromJSON Task where
    parseJSON (Object v) = Task <$>
        v .: "id" <*>
        v .: "description" <*>
        v .:? "start"

instance Show Task where
    show t = taskDesc t ++ " (" ++ show (taskId t) ++ ")"

getTopTasks :: IO [String]
getTopTasks = do
    rawText <- runProcessWithInput "task" ["due.before:tomorrow", "export"] ""
    let rawList = "[" ++ rawText ++ "]" :: String
    case decode $ BS.pack rawList of
        Just ts -> return . fmap show . filter (\t -> taskId t /= 0) $ ts
        Nothing -> return []

getCurrentTasks :: IO [String]
getCurrentTasks = do
    rawText <- runProcessWithInput "task" ["export"] ""
    let rawList = "[" ++ rawText ++ "]" :: String
    case decode $ BS.pack rawList of
        Just ts -> return . fmap show . filter (\t -> taskId t /= 0 && isJust (taskStart t)) $ ts
        Nothing -> return []

getTask :: Int -> IO String
getTask n = do
    tasks <- getTopTasks
    let n' = if n < 0 then 0 else n
    return $ if n' >= length tasks then "No task" else "Task " ++ show n' ++ ": " ++ tasks !! n'

showTask :: Int -> IO ()
showTask n = getTask n >>= updateInfoBar

showTasks :: IO ()
showTasks = do
    tasks <- getCurrentTasks
    let t = intercalate " | " . reverse . fmap (\(i, d) -> show i ++ " - " ++ d) $ zip [0..] tasks
    updateInfoBar $ "Tasks: " ++ t

getCurrentTask :: IO (Maybe Int)
getCurrentTask = do
    i <- readInfoBar
    return $ findNextTask i
    where
        findNextTask s = 
            let nt = reads . head . tail . words $ s :: [(Int, String)]
                needs = [ (length . lines $ s) == 1
                        , (length . words $ s) >= 2
                        , (head . words $ s) == "Task"
                        , not $ null nt
                        ]
            in 
                if and needs then
                    Just (fst . head $ nt)
                else Nothing

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

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
 
-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
    xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
    _ <- spawnPipe "xmobar ~/.xmonad/infobar.hs"
    xmonad $ defaults {
        logHook            = dynamicLogWithPP $ xmobarPP {
                                ppOutput = hPutStrLn xmproc
                                , ppTitle = xmobarColor "#FFB6B0" "" . shorten 100
                                , ppCurrent = xmobarColor "#CEFFAC" ""
                                , ppSep = "   "
                                }
        , manageHook = manageDocks <+> myManageHook
        , startupHook = myStartupHook
    }
 
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will 
-- use the defaults defined in xmonad/XMonad/Config.hs
-- 
-- No need to modify this.
--
defaults = defaultConfig {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        {-numlockMask        = myNumlockMask,-}
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
 
      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
 
      -- hooks, layouts
        handleEventHook    = docksEventHook,
        layoutHook         = smartBorders myLayout,
        manageHook         = myManageHook,
        startupHook        = myStartupHook
    }

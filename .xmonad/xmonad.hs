{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Char
import System.IO
import System.Exit
import System.Random
import System.Directory
import System.FilePath
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
import XMonad.Prompt.Shell
import XMonad.Util.Dmenu
import XMonad.Util.Run

-- import XMonad.Task

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal :: String
myTerminal      = "gnome-terminal --hide-menubar"
-- myTerminal      = "urxvt"

terminalRun :: MonadIO m => String -> m ()
terminalRun c = spawn $ myTerminal ++ " -x " ++ c

-- Width of the window border in pixels.
--
myBorderWidth :: Dimension
myBorderWidth   = 1

promptConfig :: XPConfig
promptConfig =
    defaultXPConfig {
        font = "xft:inconsolata:size=12:antialias=true",
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
myModMask :: KeyMask
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
{-myNumlockMask   = mod2Mask-}

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces :: [String]
myWorkspaces    =
    [ "1:web"
    , "2:code"
    , "3:chat"
    , "4:media"
    , "5"
    , "6"
    , "7:writing"
    , "8"
    , "9"
    , "xmonad"
    , "yi"
    , "network"
    , "task"
    , "bible"
    ] ++ indeedSpaces

indeedSpaces :: [String]
indeedSpaces =
    [ "gonzo"
    , "central"
    , "beaker"
    , "scooter"
    , "service"
    , "logproc"
    , "imhotep"
    , "ramses"
    , "kermit"
    , "jasx"
    , "mobile"
    , "argus"
    , "bugs"
    , "cal"
    , "eclipse"
    , "hdo"
    , "scratch"
    ]

contexts :: [ (String, X ()) ]
contexts = [ ("xmonad", xmonadContext)
           , ("network", networkContext)
           , ("task", taskContext)
           , ("poweroff", spawn "systemctl poweroff")
           , ("reboot", spawn "systemctl reboot")
           , ("code", windows (W.greedyView "2:code")
                >> spawn myTerminal >> spawn myTerminal)
           , ("bible", bibleContext)
           ]

homeRelative :: FilePath -> IO FilePath
homeRelative f = fmap (</> f) getHomeDirectory

bibleContext :: X ()
bibleContext = do
    windows $ W.greedyView "bible"
    study <- liftIO $ homeRelative "projects/esv/study.hs"
    terminalRun $ "ghci " ++ study
    plan <- liftIO $ homeRelative "projects/esv/readingplan.txt"
    terminalRun $ "vim " ++ plan

xmonadContext :: X ()
xmonadContext = do
    windows (W.greedyView "xmonad")
    isEmpty <- fmap (null . W.index . windowset) get
    if isEmpty
        then do
            xm <- liftIO $ homeRelative ".xmonad/xmonad.hs"
            terminalRun $ "ghci " ++ xm
            terminalRun $ "vim " ++ xm
        else return ()

networkContext :: X ()
networkContext = do
    windows (W.greedyView "network")
    terminalRun "wicd-curses"

taskContext :: X ()
taskContext = do
    windows (W.greedyView "task")
    terminalRun "task shell"

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor :: String
myNormalBorderColor  = "#002b36"
myFocusedBorderColor :: String
myFocusedBorderColor = "#268bd2"

dmenuArgs :: String -> [String]
dmenuArgs p =
    [ "-nb", "#002b36"
    , "-nf", "#839496"
    , "-sb", "#073642"
    , "-sf", "#cb4b16"
    , "-fn", "Inconsolata-12"
    , "-p", p
    ]

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
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
        , mapShiftKey xK_l
            $ spawn "gnome-screensaver-command --lock"
        , mapShiftKey xK_m
            $ spawn "bash ~/.us"
            {-$ spawn "setxkbmap us -option && xmodmap ~/.caps_unswap"-}
        , mapShiftKey xK_d
            $ spawn "bash ~/.dvorak"
        -- play/pause cmus
        , mapKey xK_apostrophe $ spawn "cmus-remote -u"
        -- launch dmenu
        , mapKey xK_p $ shellPrompt promptConfig
        , mapKey xK_b
            $ io getRandomBackground
            >>= spawn . ("feh --bg-scale " ++)
        -- close focused window
        , mapKey xK_c kill
        -- Rotate through the available layout algorithms
        , mapKey xK_space $ sendMessage NextLayout
        --  Reset the layouts on the current workspace to default
        , mapShiftKey xK_space $ setLayout $ XMonad.layoutHook conf
        -- Resize viewed windows to the correct size
        , mapKey xK_n refresh
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
        , mapKey xK_bracketright
            $ spawn "amixer -q set Master 3+ unmute"
        , mapKey xK_bracketleft
            $ spawn "amixer -q set Master 3- unmute"
        , mapKey xK_backslash $
            spawn "amixer -q set Master toggle" >>
            spawn "amixer -q set Headphone toggle" >>
            spawn "amixer -q set PCM toggle"
        , mapKey xK_s indeed
        , mapShiftKey xK_s shiftIndeed
        -- , mapKey xK_i $ io (getNextTask >>= showTask)
        -- , mapShiftKey xK_i $ io (getPreviousTask >>= showTask)
        -- , mapKey xK_a $ io showTasks
        -- Quit xmonad
        , mapShiftKey xK_q $ io exitSuccess
        -- Restart xmonad
        , mapKey xK_q $ restart "xmonad" True
        , mapKey xK_0 contextualize
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
        [((m .|. modMask, key), screenWorkspace sc
                >>= flip whenJust (windows . f))
            | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
            , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

shiftIndeed :: X ()
shiftIndeed = doIndeed W.shift

indeed :: X ()
indeed = doIndeed W.greedyView

doIndeed :: (String -> WindowSet -> WindowSet) -> X ()
doIndeed f = do
    s <- getSelection "PROJECT" indeedSpaces
    windows . f . takeWhile isLetter $ s

contextualize :: X ()
contextualize = do
    s <- getSelection "CONTEXT" (fmap fst contexts)
    let gs = takeWhile (/='\n') s
    case lookup gs contexts of
        Just c -> c
        _ -> return ()

getSelection :: String -> [String] -> X String
getSelection p = menuArgs "dmenu" (dmenuArgs (p++":"))

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings :: XConfig t -> M.Map (KeyMask, Button) (Window -> X ())
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
myTabConfig :: Theme
myTabConfig = defaultTheme {   activeBorderColor = "#7C7C7C"
                             , activeTextColor = "#CEFFAC"
                             , activeColor = "#000000"
                             , inactiveBorderColor = "#7C7C7C"
                             , inactiveTextColor = "#EEEEEE"
                             , inactiveColor = "#000000" }

myLayout =  onWorkspace "2:code" code $
            onWorkspace "3:chat" code $
            onWorkspace "yi" code $
            onWorkspaces indeedSpaces code $
            onWorkspace "1:web"
                (avoidStruts tiled ||| fullscreenFull Full) $
            onWorkspace "4:media"
                (avoidStruts Circle ||| fullscreenFull Full) $
            onWorkspace "7:writing" (fullscreenFull Full) $
            avoidStruts
                (tiled ||| tabbed shrinkText myTabConfig ||| Circle)
                ||| fullscreenFull Full
  where
     code = avoidStruts (tiled ||| Full)
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
myManageHook :: ManageHook
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
myStartupHook :: X ()
myStartupHook = do
    spawn "sleep 1 && xmodmap ~/.caps_swap"
    io getRandomBackground >>= spawn . ("feh --bg-scale " ++)
    setWMName "LG3D"
    {-spawn "firefox"-}

getRandomBackground :: IO FilePath
getRandomBackground = do
    home <- getHomeDirectory
    setCurrentDirectory $ home ++ "/Pictures/backgrounds"
    old <- liftM extractbg . readFile $ home ++ "/.fehbg"
    bgs <- liftM (filter ((>2) . length)) $ getDirectoryContents "."
    (i,_) <- liftM (randomR (0, length bgs - 1)) newStdGen
    new <- canonicalizePath . head . drop i $ bgs
    getHomeDirectory >>= setCurrentDirectory
    if new == old && length bgs > 1 then
        getRandomBackground
    else
        return new
    where
        extractbg = filter (/='\'') . head . drop 2 . words

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
    _ <- spawnPipe "xmobar ~/.xmonad/infobar.hs"
    xmonad $ defaults {
        logHook = dynamicLogWithPP
            $ xmobarPP  { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "#FFB6B0" ""
                            . shorten 100
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

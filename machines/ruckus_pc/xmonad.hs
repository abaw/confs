import System.Process (system)
import System.IO
import XMonad
import XMonad.Core
import qualified XMonad.StackSet as W
import XMonad.Config.Gnome
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.WindowGo
import XMonad.Actions.CycleWS
import XMonad.Actions.WindowBringer
import XMonad.Actions.GridSelect
import XMonad.Util.EZConfig
import XMonad.Util.Scratchpad
import XMonad.Util.Run
import XMonad.Layout
import XMonad.Layout.PerWorkspace
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook

main = do
  -- withFile "/tmp/x.log" AppendMode $ \tmpFile ->
  xmonad $ withUrgencyHook NoUrgencyHook
         $ gnomeConfig
       { modMask = mod5Mask
       , terminal = myTerminal
       , manageHook = myManageHook
       , focusFollowsMouse = False
       , workspaces = ["1","2","3","4","5-im","6","7","8"]
       , layoutHook = myLayoutHook
       -- , logHook = dynamicLogWithPP xmobarPP
       --             { ppOutput = appendFile "/tmp/x.log"
       --             , ppTitle = xmobarColor "green" "" . shorten 50
       --             }
       , startupHook = startupHook gnomeConfig >> dedicateTerm
       } `additionalKeysP` myKeyBindings

myKeyBindings =
    [ ("M-e", emacs)
    , ("M-b", opera)
    , ("M-S-b", chrome)
    , ("M-c", dedicateTerm)
    , ("M-s", scratchpad)
    , ("M-\\", nextScreen)
    , ("M-w", gotoMenu)
    , ("M-S-w", bringMenu)
    , ("M-u", focusUrgent)
    -- , ("M-/"), nextMatch History (return True)
    ]

myLayoutHook =  avoidStruts $ onWorkspace "5-im" imLayout standardLayout
               where
                 standardLayout = smartBorders $ layoutHook gnomeConfig
                 imLayout = gridIM (1/8) $ And (ClassName "Skype") (Role "MainWindow")

myManageHook = composeAll
               [ resource =? "filechooserdialog" --> doRectFloat (W.RationalRect 0.2 0.3 0.6 0.5)
               ] <+> scratchpadManageHookDefault <+> manageHook gnomeConfig

-- applications
myTerminal = "urxvtc"
emacs = runOrRaise "emacs" (className =? "Emacs")
opera = runOrRaise "opera" (className =? "Opera")
chrome = runOrRaise "google-chrome" (className =? "Google-chrome")
dedicateTerm = raiseMaybe (unsafeSpawn "urxvt -name urxvt-dedicate") (resource =? "urxvt-dedicate")
scratchpad = scratchpadSpawnActionTerminal myTerminal

-- drawOnFocusWindow = gets windowset >>= peek

import Control.Monad
import System.Process (system)
import System.IO
import Data.List (isPrefixOf)
import XMonad
import XMonad.Core
import XMonad.Prompt
import qualified XMonad.StackSet as W
import XMonad.Config.Gnome
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.WindowGo
import XMonad.Actions.CycleWS
import XMonad.Actions.WindowBringer
import XMonad.Actions.GridSelect
import XMonad.Actions.TagWindows
import XMonad.Actions.Submap
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Layout
import XMonad.Layout.PerWorkspace
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.Accordion
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook



main = do
  xmonad $ withUrgencyHook NoUrgencyHook
         $ myConfig

myConfig = gnomeConfig
       { modMask = mod5Mask
       , terminal = myTerminal
       , manageHook = myManageHook
       , focusFollowsMouse = False
       , workspaces = ["1","2","3","4","5-im","6-info","7","8"]
       , layoutHook = myLayoutHook
       , logHook = updateLastWindow
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
    , ("M-d", namedScratchpadAction myScratchpads "dict")
    , ("M-\\", nextScreen)
    , ("M-w", gotoMenu)
    , ("M-S-w", bringMenu)
    , ("M-u", focusUrgent)
    , ("M-/", focusLastWindow)
    , ("<F20>", mySubmap)
    ]

mySubmap = submap $ mkKeymap myConfig
           [ ("<F20>", focusLastWindow)
           , ("e", emacs)
           , ("b", opera)
           , ("S-b", chrome)
           , ("c", dedicateTerm)
           , ("C-g", return ())
           ]

myLayoutHook =  avoidStruts $
                onWorkspace "5-im" imLayout $ -- for im applications
                onWorkspace "6-info" infoLayout $ -- for emacs info frames
                standardLayout
               where
                 standardLayout = smartBorders $ layoutHook gnomeConfig
                 imLayout = gridIM (1/8) $ And (ClassName "Skype") (Role "MainWindow")
                 infoLayout = simpleDeco shrinkText defaultTheme Accordion


myManageHook = composeAll
               [ resource =? "filechooserdialog" --> doRectFloat (W.RationalRect 0.2 0.3 0.6 0.5),
                 isInfoWindow --> doShift "6-info"
               ] <+> namedScratchpadManageHook myScratchpads <+> manageHook gnomeConfig

-- applications
myTerminal = "urxvtc"
emacs = runOrRaiseNext "emacs" $ className =? "Emacs" <&&> fmap not isInfoWindow

opera = runOrRaiseNext "opera" (className =? "Opera")
chrome = runOrRaiseNext "google-chrome" (className =? "Google-chrome")
dedicateTerm = raiseMaybe (unsafeSpawn "urxvt -name urxvt-dedicate") (resource =? "urxvt-dedicate")
scratchpad = namedScratchpadAction myScratchpads "scratchpad"

-- drawOnFocusWindow = gets windowset >>= peek


updateLastWindow =
    withFocused handleFocused
    where handleFocused f = do
            sameWindow <- hasTag "current-window" f
            unless sameWindow (updateTags f)
          updateTags focused =
              do withTaggedGlobal "last-window" (delTag "last-window")
                 withTaggedGlobal "current-window" currentToLast
                 addTag "current-window" focused
              where currentToLast w =
                        delTag "current-window" w >>
                        addTag "last-window" w

focusLastWindow = focusUpTaggedGlobal "last-window"

-- scratchpads
myScratchpads =
    [
     NS "dict" "conkeror" (className =? "Conkeror") bigFloating,
     NS "scratchpad" ( myTerminal ++ " -name scratchpad") (resource =? "scratchpad") wideFloating
    ] where
    bigFloating = customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)
    wideFloating = customFloating $ W.RationalRect (1/6) (1/3) (2/3) (1/3)


-- | test if we have a emacs info frame
isInfoWindow :: Query Bool
isInfoWindow = fmap (isPrefixOf "abaw:INFO:") title

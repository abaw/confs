import Control.Monad
import System.Process (system)
import System.IO
import Data.List (isPrefixOf, stripPrefix)
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
       , workspaces = ["1","2","3","4","5","6","7","8"]
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
    , ("M-b", firefox)
    , ("M-S-b", chrome)
    , ("M-S-c", dedicateTerm)
    , ("M-c", nextTerminal)
    , ("M-s", scratchpad)
    , ("M-d", namedScratchpadAction myScratchpads "dict")
    , ("M-m", namedScratchpadAction myScratchpads "mail")
    , ("M-\\", nextScreen)
    , ("M-w", gotoMenu)
    , ("M-S-w", bringMenu)
    , ("M-u", focusUrgent)
    , ("M-/", focusLastWindow)
    , ("<F20>", mySubmap)
    ]

sharedKeyBindgs = do
  (x,y) <- myKeyBindings
  case x of
    'M': '-' : otherKeys -> return (otherKeys,y)
    _ -> mzero

mySubmap = submap $ mkKeymap myConfig $
           [ ("<F20>", focusLastWindow)
           , ("C-g", return ())
           ] ++ sharedKeyBindgs

myLayoutHook =  avoidStruts $
                standardLayout
               where
                 standardLayout = smartBorders $ ( layoutHook gnomeConfig ||| Accordion )


myManageHook = composeAll
               [ resource =? "filechooserdialog" --> doRectFloat (W.RationalRect 0.2 0.3 0.6 0.5),
                 resource =? "Ediff" --> doSideFloat NE
               ] <+> namedScratchpadManageHook myScratchpads <+> manageHook gnomeConfig

-- applications
myTerminal = "urxvt"
nextTerminal = withFocused $ \w ->
  do alreadyFocsuedTerminal <- runQuery isNormalTerminal w
     if alreadyFocsuedTerminal
       then raiseNextTerminal
       else raiseLastTerminal
  where raiseLastTerminal = raiseMaybe raiseNextTerminal isLastTerminal
        raiseNextTerminal = raiseNext isNormalTerminal >> withFocused addLastTerminalTag
        isNormalTerminal = isTerminal <&&> isNotScratchpad
        isTerminal = do
          cmd <- stringProperty "WM_COMMAND"
          return $ case (stripPrefix myTerminal cmd) of
            Nothing -> False
            Just _ -> True
        isNotScratchpad = fmap not (resource =? "scratchpad")

emacs = runOrRaiseNext "emacs" $ className =? "Emacs"

opera = runOrRaiseNext "opera" (className =? "Opera")
firefox = runOrRaiseNext (cmdRunFirefoxProfile "default") (isFirefoxWithProfile "default")
chrome = runOrRaiseNext "google-chrome" (className =? "Google-chrome")
dedicateTerm = raiseMaybe (unsafeSpawn "urxvt -name urxvt-dedicate") (resource =? "urxvt-dedicate") >> withFocused addLastTerminalTag
scratchpad = namedScratchpadAction myScratchpads "scratchpad"

cmdRunFirefoxProfile profile = "firefox -no-remote -p " ++ profile

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
     NS "dict" (cmdRunFirefoxProfile "DICT") (isFirefoxWithProfile "DICT") bigFloating,
     NS "scratchpad" ( myTerminal ++ " -name scratchpad") (resource =? "scratchpad") wideFloating,
     NS "mail" (cmdRunFirefoxProfile "MAIL") (isFirefoxWithProfile "MAIL") bigFloating
    ] where
    bigFloating = customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)
    wideFloating = customFloating $ W.RationalRect (1/6) (1/3) (2/3) (1/3)

isFirefoxWithProfile :: String -> Query Bool
isFirefoxWithProfile profile = (className =? "Firefox") <&&> fmap (isPrefixOf $ profile ++ "#") title

addLastTerminalTag :: Window -> X ()
addLastTerminalTag w = removeLastTerminalTag >> addTag "last-terminal" w
                       where removeLastTerminalTag = withTagged "last-terminal" $ delTag "last-terminal"

isLastTerminal :: Query Bool
isLastTerminal = ask >>= liftX . hasTag "last-terminal"

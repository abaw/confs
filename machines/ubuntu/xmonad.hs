{-# LANGUAGE DeriveDataTypeable #-}
import           XMonad
import           XMonad.Actions.WindowGo     (runOrRaiseNext)
import qualified XMonad.Hooks.DynamicLog     as DL
import           XMonad.Hooks.ManageDocks    (avoidStruts, manageDocks)
import           XMonad.Layout.NoBorders     (smartBorders)
import qualified XMonad.Util.ExtensibleState as XS
import           XMonad.Util.EZConfig        (additionalKeysP)
import           XMonad.Util.Run             (spawnPipe)

import           Data.Maybe                  (isNothing)
import           System.IO                   (Handle, hPutStrLn, stderr)

data MyState = MyState
        { msWindows :: [Window] -- ^ most recent windows
        } deriving Typeable

instance ExtensionClass MyState where
        initialValue = MyState
                        { msWindows = []
                        }

main = do
        xmobarH <- spawnPipe "xmobar"
        xmonad $ defaultConfig
                { modMask = mod5Mask
                , focusFollowsMouse = False
                , terminal = "urxvt"
                , manageHook = manageDocks <+> manageHook defaultConfig
                , layoutHook = myLayoutHook
                , logHook = updateXmobar xmobarH >> updateLastWindow
                } `additionalKeysP` myKeyBindings

myKeyBindings =
        [ ("M-b", chrome)
        , ("M-e", emacs)
        , ("M-/", focusLastWindow)
        ]

myLayoutHook = avoidStruts $ standardLayout
  where
        standardLayout = smartBorders $ layoutHook defaultConfig

chrome = runOrRaiseNext "google-chrome" (className =? "google-chrome")
emacs = runOrRaiseNext "emacs" (className =? "Emacs")

updateXmobar :: Handle -> X ()
updateXmobar h = DL.dynamicLogWithPP DL.xmobarPP
                        { DL.ppTitle = DL.xmobarColor "green" "" . DL.shorten 50
                        , DL.ppOutput = hPutStrLn h
                        }

updateLastWindow :: X ()
updateLastWindow = withFocused $ XS.modify . go
  where
        go w s = s { msWindows = update w (msWindows s) }
        -- remember at most 2 windows
        update w (x:y) = if w == x then x:y else w:x:y
        update w [] = w:[]

focusLastWindow :: X ()
focusLastWindow = do
        ws <- XS.gets msWindows
        case ws of
                f:w:_ -> focus w
                _ -> return ()

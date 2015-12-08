{-# LANGUAGE DeriveDataTypeable #-}
import XMonad
import XMonad.Actions.WindowGo (runOrRaiseNext)
import XMonad.Hooks.ManageDocks (manageDocks, avoidStruts)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run (spawnPipe)
import qualified XMonad.Hooks.DynamicLog as DL
import qualified XMonad.Util.ExtensibleState as XS

import Data.Maybe (isNothing)
import System.IO (hPutStrLn, Handle, stderr)

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
	, ("M-/", focusLastWindow)
	]

myLayoutHook = avoidStruts $ standardLayout
  where
	standardLayout = smartBorders $ layoutHook defaultConfig

chrome = runOrRaiseNext "google-chrome" (className =? "google-chrome")

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

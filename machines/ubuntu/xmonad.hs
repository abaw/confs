import XMonad
import XMonad.Hooks.ManageDocks
import qualified XMonad.Hooks.DynamicLog as DL
import XMonad.Util.EZConfig
import XMonad.Actions.WindowGo

main = xmonad myConfig


myConfig = defaultConfig
	{ modMask = mod5Mask
	, manageHook = myManageHook
	, layoutHook = myLayoutHook
	, logHook = DL.dynamicLogWithPP DL.xmobarPP
			{ DL.ppTitle = DL.xmobarColor "green" "" . DL.shorten 50
			}
	} `additionalKeysP` myKeyBindings

myManageHook = manageDocks <+> manageHook defaultConfig
myLayoutHook = avoidStruts $ layoutHook defaultConfig

myKeyBindings =
	[ ("M-b", chrome)
	]

chrome = runOrRaiseNext "google-chrome" (className =? "google-chrome")

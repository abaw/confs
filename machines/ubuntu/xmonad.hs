import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig
import XMonad.Actions.WindowGo

main = xmonad myConfig


myConfig = defaultConfig
	{ modMask = mod5Mask
	, manageHook = myManageHook
	, layoutHook = myLayoutHook
	} `additionalKeysP` myKeyBindings

myManageHook = manageDocks <+> manageHook defaultConfig
myLayoutHook = avoidStruts $ layoutHook defaultConfig

myKeyBindings =
	[ ("M-b", chrome)
	]

chrome = runOrRaiseNext "google-chrome" (className =? "google-chrome")

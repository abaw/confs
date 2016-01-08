{-# LANGUAGE DeriveDataTypeable #-}
import           Control.Monad               (when)
import           Data.Functor                ((<$>))
import           Data.List                   (stripPrefix)
import           Data.Maybe                  (isJust, isNothing)
import           System.IO                   (Handle, hPutStrLn, stderr)
import           XMonad
import           XMonad.Actions.GridSelect   (GSConfig (..), defaultGSConfig,
                                              goToSelected, runSelectedAction)
import           XMonad.Actions.SpawnOn      (manageSpawn, spawnAndDo)
import           XMonad.Actions.TagWindows   (addTag, hasTag, tagPrompt, unTag,
                                              withTaggedGlobal)
import           XMonad.Actions.WindowGo     (raiseMaybe, raiseNext,
                                              runOrRaiseNext)
import qualified XMonad.Hooks.DynamicLog     as DL
import           XMonad.Hooks.ManageDocks    (avoidStruts, manageDocks)
import           XMonad.Hooks.ManageHelpers  (Side (..), doRectFloat)
import           XMonad.Hooks.Place          (placeHook, simpleSmart)
import           XMonad.Layout.NoBorders     (smartBorders)
import           XMonad.Layout.PerWorkspace  (onWorkspace)
import           XMonad.Layout.Tabbed        (simpleTabbed)
import           XMonad.Prompt               (defaultXPConfig)
import qualified XMonad.StackSet             as W
import qualified XMonad.Util.ExtensibleState as XS
import           XMonad.Util.EZConfig        (additionalKeysP)
import           XMonad.Util.NamedScratchpad (NamedScratchpad (..),
                                              customFloating,
                                              namedScratchpadAction,
                                              namedScratchpadManageHook)
import           XMonad.Util.Run             (safeSpawn, spawnPipe)

data MyState = MyState
        { msWindows      :: [Window]          -- ^ most recent windows
        , msLastTerminal :: Maybe Window -- ^ last focused terminal
        } deriving Typeable

instance ExtensionClass MyState where
        initialValue = MyState
                        { msWindows = []
                        , msLastTerminal = Nothing
                        }

main = do
        xmobarH <- spawnPipe "xmobar"
        xmonad $ defaultConfig
                { modMask = mod5Mask
                , focusFollowsMouse = False
                , workspaces = ["1","2","3","4","5","6","7-im","8"]
                , terminal = myTerminal
                , manageHook = myManageHook
                , layoutHook = myLayoutHook
                , logHook = updateXmobar xmobarH >> updateLastWindow >> updateLastTerminal
		, startupHook = dedicatedTerm
                } `additionalKeysP` myKeyBindings

myTerminal = "urxvt"

myGSConfig = defaultGSConfig { gs_cellwidth = 200 }

myKeyBindings =
        [ ("M-b", chrome)
        , ("M-e", emacs)
        , ("M-/", focusLastWindow)
        , ("M-S-c", dedicatedTerm)
        , ("M-c", nextTerminal)
        , ("M-'", runSelectedAction myGSConfig menu)
        , ("M-C-c", kill)
        , ("M-m", mail)
        , ("M-d", dict)
        , ("M-s", scratchpad)
        ]

menu =
  [ ("Lock Desktop", safeSpawn "xautolock" ["-locknow"])
  , ("Add Window Tag", tagPrompt defaultXPConfig (withFocused . addTag ))
  , ("Remove Window Tags", withFocused unTag)
  , ("Volume Control", volumeControl)
  , ("All Windows", allWindows)
  ]

myManageHook = composeAll [ resource =? "filechooserdialog" --> doRectFloat (W.RationalRect 0.2 0.3 0.6 0.5)
                          , namedScratchpadManageHook myScratchpads
                          , manageDocks
                          , manageSpawn
                          , manageHook defaultConfig
                          ]
myLayoutHook = avoidStruts $ onWorkspace "7-im" imLayout $ standardLayout
  where
        standardLayout = smartBorders $ layoutHook defaultConfig
        imLayout = simpleTabbed

chrome = runOrRaiseNext "google-chrome" $ (className =? "google-chrome") <&&> (fmap not isSpecialBrowser)
firefox = runOrRaiseNext "ferefox" $ (className =? "Firefox") <&&> (fmap not isSpecialBrowser)
emacs = runOrRaiseNext "emacs" (className =? "Emacs")
dedicatedTerm = raiseMaybe (safeSpawn "urxvt" ["-name", "urxvt-dedicated"]) (resource =? "urxvt-dedicated")
mail = raiseNext isMail
dict = namedScratchpadAction myScratchpads "dict"
scratchpad = namedScratchpadAction myScratchpads "scratchpad"
volumeControl = spawnAndDo doFloat "pavucontrol"
allWindows = goToSelected defaultGSConfig

-- Jump to a terminal with these rules:
-- - if current window is not a terminal, then jump to last focused terminal.
-- - if current window is a terminal, then jump to next terminal.
-- - scratchpads are excluded from terminals
nextTerminal = withFocused $ \w -> do
                 t <- runQuery isTerminal' w
                 if t
                 then raiseNextTerminal
                 else raiseLastTerminal
  where
    raiseNextTerminal = raiseNext isTerminal'
    raiseLastTerminal = do
      w <- XS.gets msLastTerminal
      case w of
        Nothing -> raiseNextTerminal
        Just w' -> raiseMaybe raiseNextTerminal ((== w') <$> ask)

updateXmobar :: Handle -> X ()
updateXmobar h = DL.dynamicLogWithPP DL.xmobarPP
                        { DL.ppTitle = DL.xmobarColor "green" "" . DL.shorten 50
                        , DL.ppOutput = hPutStrLn h
                        }

updateLastWindow :: X ()
updateLastWindow = withFocused $ \w -> whenX (notScratch w) (updateState w)
  where
        notScratch = runQuery (fmap not isScratch)
        updateState = XS.modify . go
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

updateLastTerminal :: X ()
updateLastTerminal = withFocused $ \w -> do
                       t <- runQuery isTerminal' w
                       when t $ XS.modify (\s -> s { msLastTerminal = Just w })

isTerminal :: Query Bool
isTerminal = return . isJust . stripPrefix myTerminal =<< stringProperty "WM_COMMAND"

isTerminal' :: Query Bool
isTerminal' = isTerminal <&&> (fmap not isScratch)

isScratch :: Query Bool
isScratch = resource =? "scratchpad"

cmdScratch :: String
cmdScratch = myTerminal ++ " -name scratchpad"

isDict :: Query Bool
isDict = resource =? "dict"

cmdDict :: String
cmdDict = "uzbl --name dict http://www.ldoceonline.com/search/"

isMail :: Query Bool
isMail = hasTag' "mail"

hasTag' :: String -> Query Bool
hasTag' s = ask >>= liftX . hasTag s

isSpecialBrowser :: Query Bool
isSpecialBrowser = isMail <||> isDict

myScratchpads =
    [ NS "scratchpad" cmdScratch isScratch wideFloating
    , NS "dict" cmdDict isDict wideFloating
    ]
  where
    wideFloating = customFloating $ W.RationalRect (1/6) (1/3) (2/3) (1/3)

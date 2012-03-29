import XMonad
import XMonad.Actions.RotSlaves
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Circle
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Scratchpad
import Data.Monoid

import qualified XMonad.StackSet as W

import qualified Data.Map as M

swapWithMaster :: X ()
swapWithMaster = windows $ W.modify' $
               \c -> case c of
                     W.Stack _ [] []     -> c
                     W.Stack t [] (x:rs) -> W.Stack x [t] rs
                     W.Stack t ls rs -> W.Stack x [] (xs ++ [t] ++ rs)
                         where (x:xs) = reverse ls

myTerminal      = "urxvt"

modm = mod4Mask

myNormalBorderColor  = "#445555"

myFocusedBorderColor = "#00bbbb"

myBar = "xmobar"

myPP = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">" }

toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

myKeys = [

    -- Common apps
      ((modm,               xK_F1    ), spawn myTerminal)
    , ((modm,               xK_F2    ), scratchpadSpawnAction defaultConfig { terminal = myTerminal })
    , ((modm,               xK_F3    ), spawn "chromium")
    , ((modm,               xK_F4    ), spawn "emacs")
    , ((modm,               xK_F5    ), spawn "gmrun")

    -- Lock screen
    , ((modm,               xK_F6    ), spawn "xscreensaver-command -lock")
    -- Suspend and lock
    , ((modm .|. shiftMask, xK_F6    ), spawn "sudo pm-suspend & xscreensaver-command -lock")

    -- Colemak-adjustments

    -- Resize viewed windows to the correct size
    , ((modm,               xK_k     ), refresh)

    , ((modm,               xK_n     ), windows W.focusUp  )

    , ((modm,               xK_e     ), windows W.focusDown  )

    -- Swap the focused window with the prev window
    , ((modm .|. shiftMask, xK_n     ), windows W.swapUp  )

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_e     ), windows W.swapDown    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_i     ), sendMessage Expand)


    -- Rotate non-master windows
    , ((modm,               xK_o     ), rotSlavesUp)

    -- close focused window
    , ((modm,               xK_c     ), kill)

    , ((modm,               xK_grave ), swapWithMaster )

     ]

myManageHook = manageDocks <+> composeAll
    [ className =? "MPlayer"        --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "xmessage"       --> doCenterFloat
    , resource  =? "kdesktop"       --> doIgnore ]


myLayout = smartBorders $ avoidStruts $
         simpleTabbed ||| tiled ||| Circle
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100


main = xmonad =<< statusBar myBar myPP toggleStrutsKey defaults

defaults = defaultConfig {
       modMask            = modm
     , terminal           = myTerminal
     , normalBorderColor  = myNormalBorderColor
     , focusedBorderColor = myFocusedBorderColor
     , borderWidth        = 3
     , layoutHook         = myLayout
     , manageHook         = myManageHook
     } `additionalKeys` myKeys

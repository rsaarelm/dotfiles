{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import XMonad
import XMonad.Actions.RotSlaves
import XMonad.Hooks.ManageDocks
-- TwoPane and Spiral require RotSlaves to be really useful.
import XMonad.Layout.TwoPane
import XMonad.Layout.Spiral
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowNavigation
import XMonad.Layout.ToggleLayouts
import qualified XMonad.StackSet (focus, up, down)
import XMonad.Util.EZConfig
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "x-terminal-emulator"

-- Width of the window border in pixels.
--
myBorderWidth   = 1

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
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
myNumlockMask   = mod2Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#444444"
myFocusedBorderColor = "#00bb00"

swapWithMaster :: X ()
swapWithMaster = windows $ W.modify' $
               \c -> case c of
                     W.Stack _ [] []     -> c
                     W.Stack t [] (x:rs) -> W.Stack x [t] rs
                     W.Stack t ls rs -> W.Stack x [] (xs ++ [t] ++ rs)
                         where (x:xs) = reverse ls

-- Cycle the stack of windows up or down, maintaining focus on nth window.
cycleAllUp :: X ()
cycleAllUp = windows $ W.modify' $
             \c -> case c of
                     W.Stack _ [] []     -> c
                     W.Stack t [] (r:rs) -> W.Stack r [] (rs ++ [t])
                     W.Stack t (l:ls) [] -> W.Stack l (ls ++ [t]) []
                     W.Stack t (l:ls) (r:rs) -> W.Stack r (ls ++ [t]) (rs ++ [l])
                                                                              
cycleAllDown :: X ()
cycleAllDown = windows $ W.modify' $
               \c -> case c of
                       W.Stack _ [] []     -> c
                       W.Stack t [] rs -> W.Stack x [] (t:reverse xs)
                           where (x:xs) = reverse rs
                       W.Stack t ls [] -> W.Stack x (t:reverse xs) []
                           where (x:xs) = reverse ls
                       W.Stack t ls rs -> W.Stack x (y:reverse xs) (t:reverse ys)
                           where (x:xs) = reverse ls
                                 (y:ys) = reverse rs

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
-- The key bindings have been tweaked for the Colemak keyboard layout.
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $

  -- launch a terminal
  [ ((modMask,               xK_F1), spawn $ XMonad.terminal conf)

  -- launch dmenu
  , ((modMask,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")

  -- launch gmrun
  , ((modMask .|. shiftMask, xK_p     ), spawn "gmrun")
  , ((modMask,               xK_F2    ), spawn "gmrun")

  -- close focused window
  , ((modMask ,              xK_c     ), kill)

   -- Rotate through the available layout algorithms
  , ((modMask,               xK_space ), sendMessage NextLayout)

  --  Reset the layouts on the current workspace to default
  , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

  --  Toggle toggleable layout
  , ((modMask .|. controlMask, xK_space ), sendMessage ToggleLayout)

  -- Resize viewed windows to the correct size
  , ((modMask,               xK_n     ), refresh)

  -- Rotate non-master windows
  , ((modMask,               xK_o   ), rotSlavesUp)

  -- Move focus
  , ((modMask,               xK_Tab   ), windows W.focusDown)

  , ((modMask .|. shiftMask, xK_Tab   ), windows W.focusUp)

  -- Rotate the window stack.

  , ((modMask,               xK_n     ), cycleAllUp)

  , ((modMask,               xK_e     ), cycleAllDown)

  -- Move focus to the master window
  --, ((modMask,               xK_m     ), windows W.focusMaster  )
  , ((modMask,               xK_grave    ), swapWithMaster )

  -- Swap the focused window and the master window
  , ((modMask,               xK_Return), windows W.swapMaster)

  -- Swap the focused window with the next window
  , ((modMask .|. shiftMask, xK_n     ), windows W.swapUp)

  -- Swap the focused window with the previous window
  , ((modMask .|. shiftMask, xK_e     ), windows W.swapDown)

  -- Shrink the master area
  , ((modMask,               xK_h     ), sendMessage Shrink)

  -- Expand the master area
  , ((modMask,               xK_i     ), sendMessage Expand)

  -- Push window back into tiling
  , ((modMask,               xK_t     ), withFocused $ windows . W.sink)

  -- Increment the number of windows in the master area
  , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))

  -- Deincrement the number of windows in the master area
  , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))

  -- toggle the status bar gap
  -- TODO, update this binding with avoidStruts , ((modMask              , xK_b     ),

  -- Quit xmonad
  , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

  -- Restart xmonad
  , ((modMask              , xK_q     ), restart "xmonad" True)

  -- Lock screen
  , ((modMask,               xK_F6), spawn "xscreensaver-command -lock")
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
  -- mod-{l,u,y}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{l,u,y}, Move client to screen 1, 2, or 3
  --
  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_l, xK_u, xK_y] [0..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $

  -- mod-button1, Set the window to floating mode and move by dragging
  [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))

  -- mod-button2, Raise the window to the top of the stack
  , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))

  -- mod-button3, Set the window to floating mode and resize by dragging
  , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))

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

-- A vertical variant of TwoPane layout. Useful for monitors pivoted to
-- portrait mode.

data VertiTwoPane a =
  VertiTwoPane Rational Rational
  deriving (Show, Read)

instance LayoutClass VertiTwoPane a where
  doLayout (VertiTwoPane _ split) r s = return (arrange r s, Nothing)
    where
      arrange rect st =
        case reverse (up st) of
          (master : _) -> [(master, above), (focus st, below)]
          [] -> case down st of
            (next : _) -> [(focus st, above), (next, below)]
            [] -> [(focus st, rect)]
        where
          (above, below) = splitVerticallyBy split rect
          focus = XMonad.StackSet.focus
          up = XMonad.StackSet.up
          down = XMonad.StackSet.down

  handleMessage (VertiTwoPane delta split) x =
    return $ case fromMessage x of
      Just Shrink -> Just (VertiTwoPane delta (split - delta))
      Just Expand -> Just (VertiTwoPane delta (split + delta))
      _ -> Nothing

  description _ = "VertiTwoPane"

-- There are different TwoPane layouts for landscape and portrait monitors.
myLayout = smartBorders $ avoidStruts $ toggleLayouts (VertiTwoPane delta vRatio) (TwoPane delta hRatio) ||| simpleTabbed ||| spiral (6/7)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta hRatio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     hRatio   = 2/3

     -- Vertical pane ratio
     vRatio   = 3/4

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
myManageHook = manageDocks <+> composeAll
--myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True


------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--
myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = return ()

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = xmonad defaults

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
        numlockMask        = myNumlockMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }

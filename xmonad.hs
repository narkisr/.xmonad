import System.IO
import System.Exit
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.MultiColumns
import XMonad.Layout.Accordion
import XMonad.Layout.Dishes
import XMonad.Layout.Mosaic
import XMonad.Layout.Grid
import XMonad.Config.Mate

-- Mouse focus on activated view

import XMonad.Actions.UpdatePointer

-- See https://www.reddit.com/r/xmonad/comments/ndww5/dual_screens_multi_monitors_tips_and_tricks/
import XMonad.Layout.IndependentScreens (onCurrentScreen, withScreens, workspaces')

import XMonad.Util.Run(spawnPipe)
import qualified XMonad.StackSet as W
import qualified Data.Map        as M


------------------------------------------------------------------------
-- Terminal
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal = "/usr/bin/mate-terminal"

------------------------------------------------------------------------
-- Layouts
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = Grid ||| avoidStruts (
    Tall 1 (3/100) (1/2) |||
    tabbed shrinkText tabConfig |||
    Accordion |||
    Dishes 2 (1/6)  |||
    mosaic 2 [3,2] |||
    multiCol [1] 1 0.01 (-0.5))

-- Colors for text and backgrounds of each tab when in "Tabbed" layout.
tabConfig = defaultTheme {
    activeBorderColor = "#7C7C7C",
    activeTextColor = "#CEFFAC",
    activeColor = "#000000",
    inactiveBorderColor = "#7C7C7C",
    inactiveTextColor = "#EEEEEE",
    inactiveColor = "#000000"
}

-- window switch
multiWindowsSwitch (conf, modMask) = [((m .|. modMask, k), windows $ onCurrentScreen f i)
                                 | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
                                 , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

-- singleWindowsSwitch (conf, modMask) = [((m .|. modMask, k), windows $ f i)
--                                  | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
--                                  , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
--

------------------------------------------------------------------------
-- Key bindings

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  ----------------------------------------------------------------------
  -- Start a terminal.  Terminal to start is specified by myTerminal variable.
  [ ((modMask .|. shiftMask, xK_Return),
     spawn $ XMonad.terminal conf)

  -- Take a screenshot in select mode.
  -- After pressing this key binding, click a window, or draw a rectangle with
  -- the mouse.
  , ((modMask .|. shiftMask, xK_s),
     spawn "/usr/bin/mate-screenshot -a")

  -- Take full screenshot in multi-head mode.
  -- That is, take a screenshot of everything you see.
  , ((modMask .|. controlMask .|. shiftMask, xK_s),
     spawn "/usr/bin/mate-screenshot")

  --------------------------------------------------------------------
  -- "Standard" xmonad key bindings

  -- Close focused window.
  , ((modMask .|. shiftMask, xK_c), kill)

  -- Cycle through the available layout algorithms.
  , ((modMask, xK_space), sendMessage NextLayout)

  --  Reset the layouts on the current workspace to default.
  , ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)

  -- Resize viewed windows to the correct size.
  , ((modMask, xK_n), refresh)

  -- Move focus to the next window.
  , ((modMask, xK_Tab), windows W.focusDown)

  -- Move focus to the next window.
  , ((modMask, xK_j), windows W.focusDown)

  -- Move focus to the previous window.
  , ((modMask, xK_k), windows W.focusUp  )

  -- Move focus to the master window.
  , ((modMask, xK_m), windows W.focusMaster  )

  -- Swap the focused window with the next window.
  , ((modMask .|. shiftMask, xK_j), windows W.swapDown  )

  -- Swap the focused window with the previous window.
  , ((modMask .|. shiftMask, xK_k), windows W.swapUp    )

  -- Shrink the master area.
  , ((modMask, xK_h), sendMessage Shrink)

  -- Expand the master area.
  , ((modMask, xK_l), sendMessage Expand)

  -- Push window back into tiling.
  , ((modMask, xK_t), withFocused $ windows . W.sink)

  -- Increment the number of windows in the master area.
  , ((modMask, xK_comma), sendMessage (IncMasterN 1))

  -- Decrement the number of windows in the master area.
  , ((modMask, xK_period), sendMessage (IncMasterN (-1)))

  -- Quit xmonad.
  , ((modMask .|. shiftMask, xK_q), io (exitWith ExitSuccess))

  -- Restart xmonad.
  , ((modMask, xK_q), restart "xmonad" True)
  ]
  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  ++ multiWindowsSwitch(conf,modMask) ++
  -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings
--
-- Focus rules
-- True if your focus should follow your mouse cursor.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
    -- mod-button1, Set the window to floating mode and move by dragging
    ((modMask, button1),
     (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2),
       (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3),
       (\w -> focus w >> mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]

------------------------------------------------------------------------
-- Run xmonad with all the defaults we set up.

-- multi monitor
myWorkspaces = withScreens 2 ["1","2","3","4", "5", "6", "7", "8", "9"]
-- single monitor
-- myWorkspaces = ["1","2","3","4", "5", "6", "7", "8", "9"]

main = do
   spawn "/usr/bin/setxkbmap -option \"ctrl:nocaps\""
   xmonad mateConfig {
      workspaces     = myWorkspaces
     , terminal      = myTerminal
     , borderWidth = 2
     , focusedBorderColor = "#00FF00"
     , keys          = myKeys
     , logHook = updatePointer (0.5, 0.5) (0, 0)
     , mouseBindings = myMouseBindings
     , startupHook   = setWMName "LG3D"
     , layoutHook    = avoidStruts $ myLayout
   }

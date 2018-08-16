----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.ScreenFocusedWindow
-- Copyright   :  (c) Kevin Li
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  Kevin Li <kli6891@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A module for getting the current Window on a ScreenId
-----------------------------------------------------------------------------

module XMonad.Util.ScreenFocusedWindow
  ( currentWindow ) where

import XMonad.Core (WindowSet, ScreenId)
import XMonad.StackSet
import Control.Monad (join)


-- | Given a Workspace, grab its focused Window
focusedWindow :: Workspace i l a -> Maybe a
focusedWindow = (fmap focus) . stack 


-- | Given ScreenId, return the focused Window on that screen
currentWindow :: (Eq sid) => StackSet i l a sid sd -> sid -> Maybe a
currentWindow ws sid = 
  join $ (lookup sid things)
    where
      screens = (current ws) : (visible ws)
      things = map (\s -> (screen s, focusedWindow . workspace $ s)) screens

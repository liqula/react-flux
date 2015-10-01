{-# LANGUAGE TypeFamilies, DeriveGeneric, DeriveAnyClass #-}
-- | A store to hold the current page being viewed.
module NavStore where

import React.Flux
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Data.Typeable (Typeable)

data NavPageId = Page1
               | Page2
               | Page3
    deriving (Show, Eq, Enum, Bounded, Typeable, Generic, NFData)

allPageIds :: [NavPageId]
allPageIds = [(minBound :: NavPageId) .. ]

data NavAction = ChangePageTo NavPageId
               | ToggleSideMenu
    deriving (Show, Eq, Typeable, Generic, NFData)

data NavState = NavState
  { sideMenuOpen :: Bool
  , currentPageId :: NavPageId
  } deriving (Show, Eq, Generic, Typeable)

instance StoreData NavState where
    type StoreAction NavState = NavAction
    transform (ChangePageTo p) _ =
        -- could consider calling window.history.pushState here
        return NavState { currentPageId = p, sideMenuOpen = False }
    transform ToggleSideMenu s =
        return $ s { sideMenuOpen = not (sideMenuOpen s) } -- use a lens!

currentNavPageStore :: ReactStore NavState
currentNavPageStore = mkStore (NavState False Page1)

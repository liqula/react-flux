{-# LANGUAGE TypeFamilies, DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}
-- | A store to hold the current page being viewed.
module NavStore where

import Control.DeepSeq (NFData)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import React.Flux

import GHCJS.Types (JSRef, JSString)
import GHCJS.Foreign.Callback (Callback, syncCallback1, OnBlocked(..))
import GHCJS.Marshal (fromJSRef)

data NavPageId = Page1
               | Page2
               | Page3
    deriving (Show, Eq, Enum, Bounded, Typeable, Generic, NFData)

allPageIds :: [NavPageId]
allPageIds = [(minBound :: NavPageId) .. ]

data NavAction = ChangePageTo NavPageId -- ^ use to change the page
               | BackToPage NavPageId -- ^ used from the history handler to go back
               | ToggleSideMenu
    deriving (Show, Eq, Typeable, Generic, NFData)

data NavState = NavState
  { sideMenuOpen :: Bool
  , currentPageId :: NavPageId
  } deriving (Show, Eq, Generic, Typeable)

instance StoreData NavState where
    type StoreAction NavState = NavAction
    transform (ChangePageTo p) _ = do
        historyPushState p
        return NavState { currentPageId = p, sideMenuOpen = False }
    transform (BackToPage p) _ =
        return NavState { currentPageId = p, sideMenuOpen = False }
    transform ToggleSideMenu s =
        return $ s { sideMenuOpen = not (sideMenuOpen s) } -- use a lens!

currentNavPageStore :: ReactStore NavState
currentNavPageStore = mkStore (NavState False Page1)

--------------------------------------------------------------------------
--- History API
--------------------------------------------------------------------------

foreign import javascript unsafe
    "window['history']['pushState']({page: $1}, '', $2)"
    js_historyPushState :: Int -> JSString -> IO ()

historyPushState :: NavPageId -> IO ()
historyPushState pid = js_historyPushState (fromEnum pid) title
    where
        title = case pid of
                    Page1 -> "/page1"
                    Page2 -> "/pages/page2"
                    Page3 -> "/page3.html"

foreign import javascript unsafe
    "window['onpopstate'] = function(e) { $1(e['state'] ? e['state'].page : 0); };"
    js_setOnPopState :: Callback (JSRef -> IO ()) -> IO ()

setOnPopState :: IO ()
setOnPopState = do
    c <- syncCallback1 ContinueAsync $ \pageRef -> do
        pageInt <- fromMaybe (error "Unable to parse page") <$> fromJSRef pageRef
        alterStore currentNavPageStore $ BackToPage $ toEnum pageInt
    js_setOnPopState c

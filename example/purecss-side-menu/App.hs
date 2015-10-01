{-# LANGUAGE OverloadedStrings #-}
module App (myApp) where

import React.Flux
import NavStore
import Dispatcher
import PageViews

-- | Each page rendered by the application becomes an instance of this structure.
-- I define and fill this here inside App.hs instead of in each individual page view so
-- that the navigation definition can control how the pages are displayed and organized (dropdowns,
-- categories, etc.) in the menu.
data Page = Page
  { pageId :: NavPageId
  , pageTitle :: ReactElementM ViewEventHandler ()
    -- ^ the title showed in the sidebar menu
  , pageContent :: ReactView ()
    -- ^ the content of the page
  }

-- | Convert a page id to a page
pageFor :: NavPageId -> Page
pageFor Page1 = Page Page1 "Page 1" page1
pageFor Page2 = Page Page2 "Page 2" page2
pageFor Page3 = Page Page3 "Page 3" page3

-- | A single menu item entry in the sidebar.
menuItem_ :: NavPageId -> NavPageId -> ReactElementM ViewEventHandler ()
menuItem_ curPageId linkPageId =
    let linkPage = pageFor linkPageId
    in
    li_ [classNames [("pure-menu-item", True), ("pure-menu-selected", curPageId == pageId linkPage)]] $
        a_ ["className" $= "pure-menu-link", onClick $ \_ _ -> changePageTo $ pageId linkPage] $
            pageTitle linkPage

-- | The navigation menu
navMenu_ :: NavPageId -> ReactElementM ViewEventHandler ()
navMenu_ curPageId =
    cldiv_ "pure-menu" $ do
        span_ ["className" $= "pure-menu-heading"] "My Brand"
        ul_ ["className" $= "pure-menu-list"] $
            mapM_ (menuItem_ curPageId) allPageIds

-- | The entire layout of the app, consisting of the menu and the main content section.
myApp :: ReactView ()
myApp = defineControllerView "my application" currentNavPageStore $ \navState () -> do
    div_ ["id" $= "layout", classNames [("active", sideMenuOpen navState)]] $ do
        a_ ["id" $= "menuLink"
           , classNames [("menu-link", True), ("active", sideMenuOpen navState)]
           , onClick $ \_ _ -> [SomeStoreAction currentNavPageStore ToggleSideMenu]
           ] $ span_ mempty
        div_ ["id" $= "menu", classNames [("active", sideMenuOpen navState)]] $
            navMenu_ $ currentPageId navState
        div_ ["id" $= "main"] $
            view (pageContent $ pageFor $ currentPageId navState) () mempty

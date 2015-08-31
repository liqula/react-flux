-- |

module Main where

import           React.Flux

import qualified RouteExample.Clock      as Clock
import qualified RouteExample.EventTest  as EventTest
import           RouteExample.Router
import qualified RouteExample.TabbedApps as TabbedApps
import           RouteExample.Types

main :: IO ()
main = do
  let nestedApps = [clockApp "Clock1", clockApp "Clock2"]
  nestedViews <- mapM initApp nestedApps

  let apps = [appsToTabs "Nested Tab" nestedApps nestedViews
             , clockApp "Clock"
             , etApp "Event Tests"
             ]
  appViews <- mapM initApp apps
  let tabs = appsToTabs "main tabs" apps appViews
  tabView <- initApp tabs
  case tabs of
    App{appRouter = Just ar} -> initRouter ar
    _ -> return ()
  reactRender "route-example" tabView Nothing
  where
    appsToTabs tabsName apps appViews =
      tabApp tabsName $
      zipWith (\a v ->
                 TabbedApps.Tab (appName a) (\pr -> view v pr mempty) (appRouter a))
      apps appViews

clockApp :: String -> App TabbedApps.ParentRouter
clockApp name = App name Clock.store (\st _ -> Clock.view_ st) Clock.ClockInit Nothing

etApp :: String -> App TabbedApps.ParentRouter
etApp name = App name rst (\st _ -> EventTest.view_ st) EventTest.ETInit Nothing
  where
    rst = EventTest.store

tabApp :: String -> [TabbedApps.Tab] -> App TabbedApps.ParentRouter
tabApp name tabs =
  let rst = TabbedApps.newStore tabs
  in
    App name rst (\st rt -> TabbedApps.view_ rst rt st) TabbedApps.TabbedInit (Just $ storeRouter rst)

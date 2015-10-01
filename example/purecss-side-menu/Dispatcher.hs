module Dispatcher (
    -- re-export NavPageId so views don't have to import NavStore directly
    NavPageId(..)
  , changePageTo
) where

import React.Flux
import NavStore

changePageTo :: NavPageId -> [SomeStoreAction]
changePageTo p = [SomeStoreAction currentNavPageStore $ ChangePageTo p]

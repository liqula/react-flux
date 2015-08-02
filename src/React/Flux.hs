module React.Flux (
  -- * Stores and dispatch
    ReactStore
  , StoreData(..)
  , SomeStoreAction(..)
  , mkStore
  , dispatch
  , dispatchSomeAction

  -- * Classes
  , ReactClass
  , ViewEventHandler
  , mkControllerView
  , mkView
  , StatefulViewEventHandler
  , mkStatefulView
  , ClassEventHandler
  , mkClass

  -- * Elements
  , ReactElement
  , ReactElementM(..)
  , rclass
  , rclassWithKey
  , foreignClass
  , module React.Flux.DOM
  , module React.Flux.PropertiesAndEvents

  -- * Main
  , reactRender
) where

import Data.Typeable (Typeable)
import qualified Data.JSString as JSString

import React.Flux.Class
import React.Flux.DOM
import React.Flux.Internal
import React.Flux.PropertiesAndEvents
import React.Flux.Store

----------------------------------------------------------------------------------------------------
-- reactRender has two versions
----------------------------------------------------------------------------------------------------

-- | Render your React application into the DOM.
reactRender :: Typeable props
            => String -- ^ The ID of the HTML element to render the application into.
                      -- (This string is passed to @document.getElementById@)
            -> ReactClass props -- ^ A single instance of this class is created
            -> props -- ^ the properties to pass to the class
            -> IO ()

#ifdef __GHCJS__

reactRender htmlId rc props = do
    (e, _) <- mkReactElement id $ rclass rc props mempty
    js_ReactRender e (JSString.pack htmlId)

foreign import javascript unsafe
    "React.render($1, document.getElementById($2))"
    js_ReactRender :: ReactElementRef -> JSString.JSString -> IO ()

#else

reactRender _ _ _ = return ()

#endif

module React.Flux (
  -- * Stores and dispatch
    ReactStore
  , StoreData(..)
  , SomeStoreAction(..)
  , mkStore
  , dispatch
  , dispatchSomeAction

  -- * Views
  , ReactView
  , defineControllerView
  , defineView
  , ViewEventHandler
  , defineStatefulView
  , StatefulViewEventHandler

  -- * Elements
  , ReactElement
  , ReactElementM(..)
  , text
  , elemShow
  , view
  , viewWithKey
  , foreignClass
  , module React.Flux.DOM
  , module React.Flux.PropertiesAndEvents

  -- * Main
  , reactRender

  -- * Performance
  -- $performance
) where

import Data.Typeable (Typeable)

import React.Flux.Views
import React.Flux.DOM
import React.Flux.Internal
import React.Flux.PropertiesAndEvents
import React.Flux.Store

import GHCJS.Types (JSString)
import GHCJS.Foreign (toJSString)

----------------------------------------------------------------------------------------------------
-- reactRender has two versions
----------------------------------------------------------------------------------------------------

-- | Render your React application into the DOM.
reactRender :: Typeable props
            => String -- ^ The ID of the HTML element to render the application into.
                      -- (This string is passed to @document.getElementById@)
            -> ReactView props -- ^ A single instance of this view is created
            -> props -- ^ the properties to pass to the view
            -> IO ()

#ifdef __GHCJS__

reactRender htmlId rc props = do
    (e, _) <- mkReactElement id $ view rc props mempty
    js_ReactRender e (toJSString htmlId)

foreign import javascript unsafe
    "React['render']($1, document.getElementById($2))"
    js_ReactRender :: ReactElementRef -> JSString -> IO ()

#else

reactRender _ _ _ = return ()

#endif

-- $performance 
--
--
-- The 'NFData' instance is used for a small optimization in event handlers.  React.js keeps event
-- objects (the object passed to the handlers) in a pool and re-uses them for successive events.
-- We parse this event object lazily so that only properties actually accessed are parsed, and then
-- use 'NFData' instance to force the evaluation of the store action(s) resulting from the event.
-- We can then compute the action before the event object returns to the React pool.

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
  , defineStatefulView
  , ViewEventHandler
  , StatefulViewEventHandler

  -- * Elements
  , ReactElement
  , ReactElementM(..)
  , elemText_
  , elemShow_
  , view
  , viewWithKey
  , ReactViewKey
  , childrenPassedToView_
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
    (e, _) <- mkReactElement id (return []) $ view rc props mempty
    js_ReactRender e (toJSString htmlId)

foreign import javascript unsafe
    "React['render']($1, document.getElementById($2))"
    js_ReactRender :: ReactElementRef -> JSString -> IO ()

#else

reactRender _ _ _ = return ()

#endif

-- $performance 
--
-- React obtains high <https://facebook.github.io/react/docs/advanced-performance.html performance> from two techniques: the
-- <https://facebook.github.io/react/docs/reconciliation.html virtual DOM/reconciliation> and 
-- <https://facebook.github.io/react/docs/events.html event handlers> registered on the document.
--
-- To support fast reconciliation, React uses key properties (set by 'viewWithKey') and a
-- @shouldComponentUpdate@ lifetime class method.  The React documentation on
-- <https://facebook.github.io/react/docs/advanced-performance.html performance and immutable-js> talks
-- about using persistent data structures, which is exactly what Haskell does.  Therefore, we
-- implement a @shouldComponentUpdate@ method which compares if the javascript object representing
-- the Haskell values for the @props@, @state@, and/or @storeData@ have changed.  Thus if you do not
-- modify a Haskell value that is used for the @props@ or @state@ or @storeData@, React will skip
-- re-rendering that view instance.  Care should be taken in the 'transform' function to not edit or
-- recreate any values that are used as @props@.  For example, instead of something like
--
-- >[ (idx, todo) | (idx, todo) <- todos, idx /= deleteIdx ]
--
-- you should prefer
--
-- >filter ((/=deleteIdx) . fst) todos
-- 
-- After this transform, the list of todos has changed so @mainSection@ will be re-rendered by
-- React.  @mainSection@ calls @todoItem@ with the tuple @(idx,todo)@ as the props. In the latter
-- transform snippet above, the tuple value for the entries is kept unchanged, so the
-- @shouldComponentUpdate@ function for @todoItem@ will return false and React will not re-render
-- each todo item.  If instead the tuple had been re-created as in the first snippet, the underlying
-- javascript object will change even though the value is equal.  The @shouldComponentUpdate@
-- function for @todoItem@ will then return true and React will re-render every todo item.  Thus the
-- latter snippet is preferred.  In summary, if you are careful to only update the part of the store
-- data that changed, React will only re-render those part of the page.
--
-- For events, React registers only global event handlers and also keeps event objects (the object
-- passed to the handlers) in a pool and re-uses them for successive events.  We want to parse this
-- event object lazily so that only properties actually accessed are parsed, but this is a problem
-- because lazy access could occur after the event object is reused.  Instead of making a copy of
-- the event, we use the 'NFData' instance on 'SomeStoreAction' to force the evaluation of the store
-- action(s) resulting from the event.  We therefore compute the action before the event object
-- returns to the React pool, and rely on the type system to prevent the leak of the event object
-- outside the handlers.  Thus, you cannot "cheat" in the 'NFData' instance on your store actions;
-- the event objects dilerbertly do not have a 'NFData' instance, so that you must pull all your
-- required data out of the event object and into an action in order to properly implement 'NFData'.
-- Of course, the easiest way to implement 'NFData' is to derive it with Generic and DeriveAnyClass,
-- as @TodoAction@ does above.

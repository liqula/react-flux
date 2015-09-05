-- | A binding to <https://facebook.github.io/react/index.html React> based on the
-- <https://facebook.github.io/flux/docs/overview.html Flux> design.  The flux design pushes state
-- and complicated logic out of the view, allowing the rendering functions and event handlers to be
-- pure Haskell functions.  When combined with React's composable components and the one-way flow of
-- data, React, Flux, and GHCJS work very well together.
--
-- __Prerequisites__: This module assumes you are familiar with the basics of React and Flux.  From
-- the <https://facebook.github.io/react/docs/tutorial.html React documentation>, you should read at
-- least \"Tutorial\", \"Displaying Data\", \"Multiple Components\", and \"Forms\".  Note that
-- instead of JSX we use a Writer monad, but it functions very similarly so the examples in the
-- React documentation are very similar to how you will write code using this module.  The other
-- React documentation you can skim, the Haddocks below link to specific sections of the React
-- documentation when needed.  Finally, you should read the
-- <https://facebook.github.io/flux/docs/overview.html Flux overview>, in particular the central
-- idea of one-way flow of data from actions to stores to views which produce actions.
--
-- __Organization__: Briefly, you should create one module to contain the dispatcher, one module for
-- each store, and modules for the view definitions.  These are then imported into a Main module,
-- which calls 'reactRender' and initializes any AJAX load calls to the backend. The source package
-- contains some <https://bitbucket.org/wuzzeb/react-flux/src/tip/example/ example applications>.
--
-- __Deployment__: Care has been taken to make sure closure with ADVANCED_OPTIMIZATIONS correctly
-- minimizes a react-flux application.  No externs are needed, instead all you need to do is provide
-- or protect the @React@ variable.  The TODO example does this as follows:
--
-- >(function(global, React) {
-- >contents of all.js
-- >})(this, window['React']);
--
-- __Testing__:  I use the following approach to test my react-flux application.  First, I use unit
-- testing to test the dispatcher and store 'transform' functions.  Since the dispatcher and the
-- store transform are just data manipulation, existing Haskell tools like hspec, QuickCheck,
-- SmallCheck, etc. work well.  Note that stores and 'dispatch' work in GHC and GHCJS, so this unit
-- testing can be done either in GHC or GHCJS. I don't do any unit testing of the views, because any
-- complicated logic in event handlers is moved into the dispatcher and the
-- rendering function is difficult to test in isolation.  Instead, I test the rendering via
-- end-2-end tests using <https://hackage.haskell.org/package/hspec-webdriver hspec-webdriver>.
-- This tests the React frontend against the real backend and hspec-webdriver has many utilities for
-- easily checking that the DOM is what you expect.  I have found this much easier than trying to
-- unit test each view individually, and you can still obtain the same coverage for equal effort.
-- The file <https://bitbucket.org/wuzzeb/react-flux/src/tip/test/spec/TodoSpec.hs test\/spec\/TodoSpec.hs>
-- in the source code contains a hspec-webdriver test for the TODO example application.

module React.Flux (
  -- * Dispatcher
  -- $dispatcher

  -- * Stores
    ReactStore
  , StoreData(..)
  , mkStore
  , getStoreData
  , alterStore
  , SomeStoreAction(..)
  , executeAction

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
  , elemText
  , elemShow
  , view
  , viewWithKey
  , ReactViewKey
  , childrenPassedToView
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

#ifdef __GHCJS__
import GHCJS.Types (JSString)
#endif

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
-- After either of these transforms, the list of todos has changed so @mainSection@ will be re-rendered by
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

-- $dispatcher
-- The dispatcher is the central hub that manages all data flow in a Flux application.  It has no
-- logic of its own and all it does is distribute actions to stores.  There is no special support
-- for a dispatcher in this module, since it can be easily implemented directly using Haskell
-- functions.  The event handlers registered during rendering are expected to produce a list of 'SomeStoreAction'.
-- The dispatcher therefore consists of Haskell functions which produce these lists of
-- 'SomeStoreAction'.  Note that this list of actions is used instead of @waitFor@ to sequence
-- actions to stores: when dispatching, we wait for the 'transform' of each action to complete
-- before moving to the next action.
--
-- In the todo example application there is only a single store, so the dispatcher just
-- passes along the action to the store.  In a larger application, the dispatcher could have its
-- own actions and produce specific actions for each store.
--
-- >dispatchTodo :: TodoAction -> [SomeStoreAction]
-- >dispatchTodo a = [SomeStoreAction todoStore a]

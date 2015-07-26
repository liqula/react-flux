-- | Internal module containing the class definitions
module React.Flux.Class (
    ReactClass(..)
  , mkControllerView
  , mkView
  , ViewEventHandler
  , mkStatefulView
  , StatefulViewEventHandler
  , mkClass
  , ClassEventHandler
  , rclass
  , rclassWithKey
) where

import Data.Aeson
import Data.Typeable (Typeable)
import Control.Monad.Writer (runWriter)

import React.Flux.Store
import React.Flux.Element
--import React.Flux.JsTypes
--import React.Flux.Events

-- | A React class is conceptually a (stateful) function from properties to a tree of elements.
--
-- This module supports 3 kinds of pure classes and 1 unpure class.
--
-- * Controller View, created by 'mkControllerView'.  A controller view provides the glue code
-- between a store and the view, and as such is a pure function taking as input the store data and
-- the properties and producing a tree of elements.  In addition, any event handlers attached to
-- elements can only produce actions.
--
-- * View.  A view is pure function from props to a tree of elements.  It can eiter be modeled by
-- just a Haskell function without a 'ReactClass', or as a 'ReactClass' created by 'mkView'.  Using
-- the machinery of a 'ReactClass' is helpful because it allows React to more easily reconcile the
-- virtual DOM with the DOM and leads to faster rendering (as long as you use 'rclassWithKey'
-- when creating an instance of the view).
--
-- * Stateful View, created by 'mkStatefulView'.  A stateful view is a class which keeps track of
-- some internal state, but it is impossible for the state to leak outside of the class.  It
-- consists of a pure function taking as input the properties and current state and producing a tree
-- of elements.  Event handlers registered on elements can transform the state and produce actions,
-- but cannot perform any other @IO@.
--
-- The above classes are pure in the sense that this module enforces at the type level that no @IO@
-- can occur either during rendering or event handlers.  The only @IO@ that occurs is inside of the
-- 'transform' function on the store. Occasionally, you may need to create a class that performs
-- arbitrary IO either during rendering or event handlers, and this module does provide an
-- escape-hatch in 'mkClass'.

#ifdef __GHCJS__
newtype ReactClass props = ReactClass { reactClassRef :: (ReactClassRef props) }
#else
data ReactClass props =
    forall storeData. Typeable storeData =>
        TestReactControllerView String (storeData -> props -> ReactElement ViewEventHandler)

  | TestReactView String (props -> ReactElement ViewEventHandler)

  | forall state. (ToJSON state, FromJSON state) =>
        TestReactStatefulView String (state -> props -> ReactElement (StatefulViewEventHandler state))

  | forall state. (ToJSON state, FromJSON state) =>
        TestReactClass String (state -> props -> IO (ReactElement (ClassEventHandler state)))

reactClassRef :: ReactClass props -> String
reactClassRef (TestReactControllerView n _) = n
reactClassRef (TestReactView n _) = n
reactClassRef (TestReactStatefulView n _) = n
reactClassRef (TestReactClass n _) = n
#endif

---------------------------------------------------------------------------------------------------
--- Two versions of mkControllerView
---------------------------------------------------------------------------------------------------

-- | Event handlers in a controller-view and a view transform events into actions, but are not
-- allowed to perform any 'IO'.
type ViewEventHandler = [SomeStoreAction]

-- | A controller view provides the glue between a 'ReactStore' and the DOM.
--
-- The controller-view registers with the given store.  Whenever the store is transformed, the
-- controller-view re-renders itself.  It is recommended to have one controller-view for each
-- significant section of the page.  Controller-views deeper in the page tree can cause complexity
-- because data is now flowing into the page in multiple possibly conflicting places.  You must
-- balance the gain of encapsulated components versus the complexity of multiple entry points for
-- data into the page.  Deeper components in the page can still be made re-usuable by using views or
-- stateful views.
--
-- Each instance of a controller-view also accepts properties of type @props@ from its parent.
-- Whenever the parent re-renders itself, the new properties will be passed down to the
-- controller-view causing it to re-render itself.
--
-- Events registered on controller-views just produce actions, which get dispatched to the
-- appropriate store which causes the store to transform itself, which eventually leads to the
-- controller-view re-rendering.  This one-way flow of data from actions to store to
-- controller-views is central to the flux design.
--
-- While the above re-rendering on any store data or property change is conceptually what occurs,
-- React uses a process of <https://facebook.github.io/react/docs/reconciliation.html
-- reconciliation> to speed up re-rendering.  The best way of taking advantage of re-rendering is to
-- use key properties with 'rclassWithKey'.
--
-- TODO

#ifdef __GHCJS__

mkControllerView :: (StoreData storeData, Typeable props)
                 => String -- ^ A name for this class
                 -> ReactStore storeData -- ^ The store this controller view should attach to.
                 -> (storeData -> props -> ReactElement ViewEventHandler) -- ^ The rendering function
                 -> ReactClass props
mkControllerView name (ReactStore store) buildNode = unsafePerformIO $ do
    renderCb <- syncCallback1 ThrowWouldBlock $ \arg -> do

        storeDataE <- js_RenderCbRetrieveState arg
        mdata <- derefExport storeDataE
        storeData <- maybe (error "Unable to load store state") return mdata

        propsE <- js_RenderCbRetrieveProps arg
        mprops <- derefExport propsE
        props <- maybe (error "Unable to load props") return mprops

        let node = fmap runViewHandler $ buildNode storeData props
        (element, evtCallbacks) <- runWriterT $ renderNode node

        evtCallbacksRef <- toJSArg evtCallbacks
        js_RenderCbSetResults arg evtCallbacks element

    releaseCb <- syncCallback1 ThrowWouldBlock releaseCallback

    releaseProps <- syncCallback1 ThrowWouldBlock releaseExport

    ReactClass <$> js_createControllerView (toJSString name) store renderCb releaseCb releaseProps

-- | Transform a controller view handler to a raw handler.
runViewHandler :: ViewEventHandler -> IO ()
runViewHandler = mapM_ dispatchSomeAction

#else

mkControllerView :: (StoreData storeData, Typeable props)
                 => String -- ^ A name for this class
                 -> ReactStore storeData -- ^ The store this controller view should attach to.
                 -> (storeData -> props -> ReactElement ViewEventHandler) -- ^ The rendering function
                 -> ReactClass props
mkControllerView n _ f = TestReactControllerView n f

#endif

---------------------------------------------------------------------------------------------------
--- Two versions of mkView
---------------------------------------------------------------------------------------------------

-- | A view is a re-usable component of the page which does not track any state itself.
--
-- Each instance of a view accepts properties of type @props@ from its parent and re-renders itself
-- whenever the properties change.
--
-- One option to implement views is to just use a Haskell function taking the @props@ as input and
-- produce a 'ReactElementM'.  For small views, such a Haskell function is ideal.
--
-- Using a view class provides more than just a Haskell function when used with a key property with
-- 'rclassWithKey'.  The key property allows React to more easily reconcile the virtual DOM with the
-- browser DOM.

#ifdef __GHCJS__

mkView :: Typeable props
       => String -- ^ A name for this class
       -> (props -> ReactElement ViewEventHandler) -- ^ The rendering function
       -> ReactClass props
mkView name buildNode = unsafePerformIO $ do
    renderCb <- syncCallback1 ThrowWouldBlock $ \arg -> do

        propsE <- js_RenderCbRetrieveProps arg
        mprops <- derefExport propsE
        props <- maybe (error "Unable to load props") return mprops

        let node = fmap expandViewHandler $ buildNode props
        (element, evtCallbacks) <- runWriterT $ renderNode node

        evtCallbacksRef <- toJSArg evtCallbacks
        js_RenderCbSetResults arg evtCallbacks element

    releaseCb <- syncCallback1 ThrowWouldBlock releaseCallback
    releaseProps <- syncCallback1 ThrowWouldBlock releaseExport

    ReactClass <$> js_createView (toJSString name) renderCb releaseCb releaseProps

#else

mkView :: Typeable props
       => String -- ^ A name for this class
       -> (props -> ReactElement ViewEventHandler) -- ^ The rendering function
       -> ReactClass props
mkView = TestReactView

#endif

---------------------------------------------------------------------------------------------------
--- Two versions of mkStatefulView
---------------------------------------------------------------------------------------------------

-- | A stateful class event handler transforms events into store actions and a new state.
-- If the new state is nothing, no change is made to the state (which allows an optimization in that
-- we do not need to re-render the view).
type StatefulViewEventHandler state = ([SomeStoreAction], Maybe state)

-- | A stateful view is a re-usable component of the page which keeps track of internal state.
--
-- The rendering function is a pure function of the state and the properties from the parent.  The
-- view will be re-rendered whenever the state or properties change.  The only way to
-- transform the internal state of the view is via an event handler, which can optionally produce
-- new state.
--
-- TODO

#ifdef __GHCJS__

mkStatefulView :: (ToJSON state, FromJSON state, Typeable props)
               => String -- ^ A name for this class
               -> state -- ^ The initial state
               -> (state -> props -> ReactElement (StatefulViewEventHandler state)) -- ^ The rendering function
               -> ReactClass props
mkStatefulView name initial render = mkClassHelper runStateViewHandler name initial render'
    where
        render' state props = return $ render state props

-- | Transform a stateful class event handler to a raw event handler
runStateViewHandler :: ToJSON state => SetStateFn -> StatefulViewEventHandler state -> IO ()
runStateViewHandler setStateFn (actions, mNewState) = do
    case mNewState of
        Nothing -> return ()
        Just newState -> do
            newStateRef <- toJSRef_aeson newState
            js_SetState setStateFn newStateRef
    mapM_ dispatchSomeAction actions

#else

mkStatefulView :: (ToJSON state, FromJSON state, Typeable props)
               => String -- ^ A name for this class
               -> state -- ^ The initial state
               -> (state -> props -> ReactElement (StatefulViewEventHandler state)) -- ^ The rendering function
               -> ReactClass props
mkStatefulView n _ f = TestReactStatefulView n f

#endif

---------------------------------------------------------------------------------------------------
--- Two versions of mkClass
---------------------------------------------------------------------------------------------------

-- | A class event handler can perform arbitrary IO, producing the new state.  If the new state is
-- nothing, no change is made to the state (which allows an optimization in that we do not need to
-- re-render the view).
type ClassEventHandler state = IO (Maybe state)

-- | Create a class allowed to perform arbitrary IO during rendering and event handlers.

#ifdef __GHCJS__

mkClass :: (ToJSON state, FromJSON state, Typeable props)
        => String -- ^ A name for this class
        -> state -- ^ The initial state
        -> (state -> props -> IO (ReactElement (ClassEventHandler state))) -- ^ The rendering function
        -> ReactClass props
mkClass = mkClassHelper expandClassHandler

-- | Transform a class event handler to a raw event handler.
runClassHandler :: (FromJSON state, ToJSON state)
                => SetStateFn -> ClassEventHandler state -> IO ()
runClassHandler setStateFn handler = do
    mNewState <- handler
    case mNewState of
        Nothing -> return ()
        Just newState -> do
            newStateRef <- toJSRef_aeson newState
            js_SetState setState newStateRef

#else

mkClass :: (ToJSON state, FromJSON state, Typeable props)
        => String -- ^ A name for this class
        -> state -- ^ The initial state
        -> (state -> props -> IO (ReactElement (ClassEventHandler state))) -- ^ The rendering function
        -> ReactClass props
mkClass n _ f = TestReactClass n f

#endif


---------------------------------------------------------------------------------------------------
--- Various GHCJS only utilities
---------------------------------------------------------------------------------------------------

#ifdef __GHCJS__

-- | The class render callback is a haskell function given to the javascript class
-- that is called every time the class is to be rendered.  The argument to this callback is
-- javascript object used both for input and output.  The properties of this object are:
--
-- * state :: Export state
-- * props :: Export props
-- * newCallbacks :: [Callback].  This array is set by Haskell, and contains all the event
--       callbacks created as part of the rendering.  These callbacks will be stored and
--       after the next render, these callbacks will be freed.
-- * elem  This is set by Haskell and contains the value that should be returned by the render
--         function back to React.
--
--  In addition, for classes (not controller-views), there is one additional property @setState@
--  which is used inside the event handlers.
data RenderCbArgs

foriegn import javascript unsafe
    "$1.state"
    js_RenderCbRetrieveState :: JSRef RenderCbArgs -> IO (JSRef state)

foreign import javascript unsafe
    "$1.props"
    js_RenderCbRetrieveProps :: JSRef RenderCbArgs -> IO (Export props)

foreign import javascript unsafe
    "$1.newCallbacks = $2; $1.elem = $3;"
    js_RenderCbSetResults :: JSRef RenderCbArgs -> JSRef a -> JSRef b -> IO ()

type SetStateFn = JSRef (JSRef state -> IO ())

foreign import javascript unsafe
    "$1.alterState"
    js_RenderCbRetrieveSetStateFn :: JSRef RenderCbArgs -> IO SetStateFn

foreign import javascript unsafe
    "$1($2)"
    js_SetState :: SetStateFn -> JSRef state -> IO ()

-- | Create a controller view.  On mounting and unmounting, register with the store to be notified
-- of state changes.  Any time the properties change they must be released, so @releaseProps@
-- is 'releaseExport' exported to javascript.  Also, the event callbacks created during a render
-- must eventually be cleaned up.  So 'releaseCb' is 'releaseCallback' exported to javascript, and
-- is used after rendering to clean up the callbacks from the previous render.
--
-- Also, the haskell props of type @Export props@ are in the @hs@ property of the React props.  This
-- is so that we can pass a javascript object with reserved config entries like @key@ in the call to
-- createElement.
foreign import javascript unsafe
    "(function(name, store, renderCb, releaseCb, releaseProps) {
        React.createClass({ \
            displayName: name,
            getInitialState: function() { \
                return store.sdata; \
            }, \
            componentDidMount: function() { \
                store.views.push(this.setState); \
            }, \
            componentWillUnmount: function() {
                var idx = store.views.indexOf(this.setState); \
                if (idx >= 0) { store.views.splice(idx, 1); } \
                this._currentCallbacks.map(releaseCb);
                releaseProps(this.props.hs);
            }, \
            componentWillReceiveProps: function() { \
                releaseProps(this.props.hs); \
            }, \
            render: function() { \
                var arg = { \
                    state: this.state, \
                    props: this.props.hs, \
                    newCallbacks: [], \
                    elem:null, \
                }; \
                renderCb(arg); \
                this._currentCallbacks.map(releaseCb); \
                this._currentCallbacks = arg.newCallbacks; \
                return arg.elem; \
            }, \
            _currentCallbacks: [], \
        }) \
    })($1, $2, $3, $4, $5)"
    js_createControllerView :: JSString
                           -> ReactStoreRef storeData
                           -> (JSFun (JSRef RenderCbArgs -> IO ()))
                           -> (JSFun (Callback a -> IO ()))
                           -> (JSFun (Export a -> IO ())
                           -> IO (ReactClassRef props)

-- | Create a class with no state.
foreign import javascript unsafe
    "(function(name, renderCb, releaseCb, releaseProps) {
        React.createClass({ \
            displayName: name,
            componentWillUnmount: function() {
                this._currentCallbacks.map(releaseCb);
                releaseProps(this.props.hs);
            }, \
            componentWillReceiveProps: function() { \
                releaseProps(this.props.hs); \
            }, \
            render: function() { \
                var arg = { \
                    props: this.props.hs, \
                    newCallbacks: [], \
                    elem:0, \
                }; \
                renderCb(arg); \
                this._currentCallbacks.map(releaseCb); \
                this._currentCallbacks = arg.newCallbacks; \
                return x.r; \
            }, \
            _currentCallbacks: [], \
        }) \
    })($1, $2, $3, $4)"
    js_createView :: JSString
                  -> JSFun (JSRef RenderCbArgs -> IO ())
                  -> JSFun (Callback a -> IO ())
                  -> JSFun (Export a -> IO ())
                  -> IO (ReactClassRef props)

-- | Create a class which tracks its own state.  Similar releasing needs to happen for callbacks and
-- properties as for controller views.
foreign import javascript unsafe
    "(function(name, initialState, renderCb, releaseCb, releaseProps) {
        React.createClass({ \
            displayName: name,
            getInitialState: function() { \
                return initialState;
            }, \
            componentWillUnmount: function() {
                this._currentCallbacks.map(releaseCb);
                releaseProps(this.props.hs);
            }, \
            componentWillReceiveProps: function() { \
                releaseProps(this.props.hs); \
            }, \
            render: function() { \
                var arg = { \
                    state: this.state, \
                    props: this.props.hs, \
                    newCallbacks: [], \
                    elem:null, \
                    setState: this.setState, \
                }; \
                renderCb(arg); \
                this._currentCallbacks.map(releaseCb); \
                this._currentCallbacks = arg.newCallbacks; \
                return x.r; \
            }, \
            _currentCallbacks: [], \
        }) \
    })($1, $2, $3, $4, $5)"
    js_createClass :: JSString
                   -> JSRef state
                   -> JSFun (JSRef RenderCbArgs -> IO ())
                   -> JSFun (Callback a -> IO ())
                   -> JSFun (Export a -> IO ())
                   -> IO (ReactClassRef props)

-- | Helper function used for 'mkStatefulView' and 'mkClass'
mkClassHelper :: (ToJSON state, FromJSON state, Typeable props)
              => (AlterStateFns -> eventHandler -> IO ()))
              -> String
              -> state
              -> (state -> props -> IO (ReactElement eventHandler))
              -> ReactClass props
mkClassHelper runHandler name initial render = unsafePerformIO $ do

    initialRef <- toJSRef_aeson initial

    renderCb <- syncCallback1 ThrowWouldBlock $ \arg -> do

        stateRef <- js_RenderCbRetrieveState arg
        mstate <- fromJSRef stateRef
        stateVal <- maybe (error "Unable to decode class state") return mstate
        state <- case fromJSON stateVal of
                    Error err -> error $ "Unable to decode class state: " ++ err
                    Success s -> return s

        propsE <- js_RenderCbRetrieveProps arg
        mprops <- derefExport propsE
        props <- maybe (error "Unable to load props") return mprops

        alterStateFns <- js_RenderCbRetrieveAlterStateFns
        node <- fmap (runHandler alterStateFns) <$> render state props

        (element, evtCallbacks) <- runWriterT $ renderNode node

        evtCallbacksRef <- toJSArg evtCallbacks
        js_RenderCbSetResults arg evtCallbacks element

    releaseCb <- syncCallback1 ThrowWouldBlock releaseCallback

    releaseProps <- syncCallback1 ThrowWouldBlock releaseExport

    ReactClass <$> js_createClass (toJSString name) initialRef renderCb releaseCb releaseProps
#endif

----------------------------------------------------------------------------------------------------
--- Element creation for classes
----------------------------------------------------------------------------------------------------

-- | Create an element from a class.  I suggest you make a combinator for each of your classes.  For
-- example,
--
-- TODO
rclass :: Typeable props => ReactClass props -- ^ the class
                         -> props -- ^ the properties to pass into the instance of this class
                         -> ReactElementM eventHandler a -- ^ The children of the element
                         -> ReactElementM eventHandler a
rclass rc props (ReactElementM child) =
    let (a, childEl) = runWriter child
     in elementToM a $ ClassElement (reactClassRef rc) (Nothing :: Maybe ()) props childEl

-- | Create an element from a class, and also pass in a key property for the instance.  Key
-- properties speed up the <https://facebook.github.io/react/docs/reconciliation.html reconciliation>
-- of the virtual DOM with the DOM.  The key does not need to be globally unqiue, it only needs to
-- be unique within its siblings, not globally unique.
--
-- TODO
rclassWithKey :: (Typeable props, ToJSON key) => ReactClass props -- ^ the class
                                              -> key -- ^ A value unique within the siblings of this element
                                              -> props -- ^ The properties to pass to the class instance
                                              -> ReactElementM eventHandler a -- ^ The children of the class
                                              -> ReactElementM eventHandler a
rclassWithKey rc key props (ReactElementM child) =
    let (a, childEl) = runWriter child
     in elementToM a $ ClassElement (reactClassRef rc) (Just key) props childEl

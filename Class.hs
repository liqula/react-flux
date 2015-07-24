module React.Flux.Class (
    ReactClass_
  , ReactClassRef
  , ReactClass(..)
  , ControllerViewEventHandler
  , mkControllerView
  , StatefulClassEventHandler
  , mkStatefulClass
  , ClassEventHandler
  , mkClass
) where

-- | This type is for the return value of React.createClass.
data ReactClass_
type ReactClassRef props = JSRef ReactClass_

-- | A React class is conceptually a (stateful) function from properties to a tree of elements.
--
-- This module supports 3 kinds of pureish classes and 1 unpure class.
--
-- * Controller View.  The controller view provides the glue code between a store and the view, and
-- as such is a pure function taking as input the store data and the properties and producing a tree
-- of elements.  In addition, any event handlers attached to elements can only produce actions.
--
-- * View.  A view is pure function from props to a tree of elements.  It is best modeled by just a
-- Haskell function, so a 'ReactClass' is not needed.
--
-- * Stateful Class.  A stateful class is a class which keeps track of some internal state, but it
-- is impossible for the state to leak outside of the class.  It consists of a pure function taking
-- as input the properties and current state and producing a tree of elements.  Event handlers
-- registered on elements can transform the state and produce actions, but cannot perform any other
-- actions.
--
-- The above classes I call pureish because the this module enforces at the type level that no @IO@
-- can occur either during rendering or event handlers (of course under the hood there are many
-- side-effects).  Occasionally, you may need to create a class that performs arbitrary IO either
-- during rendering or event handlers, and this module does provide an escape-hatch in 'mkClass'.
newtype ReactClass props = ReactClass (ReactClassRef_ props)

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
--  In addition, for classes (not controller-views), there are two additional properties, getState
--  and setState.  These two functions are sent from the javascript to Haskell for use inside the
--  event handlers.
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

type GetStateFn = JSRef (IO (JSRef state))
type SetStateFn = JSRef (JSRef state -> IO ())

foreign import javascript unsafe
    "$1.getState"
    js_RenderCbRetrieveGetStateFn :: JSRef RenderCbArgs -> IO GetStateFn

foreign import javascript unsafe
    "$1.setState"
    js_RenderCbRetrieveSetStateFn :: JSRef RenderCbArgs -> IO SetStateFn

foreign import javascript unsafe
    "$1()"
    js_GetState :: GetStateFn -> IO (JSRef state)

foreign import javascript unsafe
    "$1($2)"
    js_SetState :: GetStateFn -> JSRef state -> IO ()

-- | Create a controller view.  On mounting and unmounting, register with the store to be notified
-- of state changes.  Any time the properties change they must be released, so @releaseProps@
-- is 'releaseExport' exported to javascript.  Also, the event callbacks created during a render
-- must eventually be cleaned up.  So 'releaseCb' is 'releaseCallback' exported to javascript, and
-- is used after rendering to clean up the callbacks from the previous render.
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
                releaseProps(this.props);
            }, \
            componentWillReceiveProps: function() { \
                releaseProps(this.props); \
            }, \
            render: function() { \
                var arg = { \
                    state: this.state, \
                    props: this.props, \
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

-- | Event handlers in a controller view transform events into actions.
type ControllerViewEventHandler = Event -> [SomeStoreAction]

-- | Transform a controller view handler to a raw handler.
expandCtrlViewHandler :: ControllerViewEventHandler -> RawEventHandler
expandCtrlViewHandler handler evt = mapM_ dispatchSome $ handler evt
    where
        dispatchSome (SomeStoreAction store action) = dispatch store action

-- | A controller view provides the glue between a 'ReactStore' and the DOM.
--
-- TODO
mkControllerView :: Typeable props
                 => String -- ^ A name for this class
                 -> ReactStore storeData -- ^ The store this controller view should attach to.
                 -> (storeData -> props -> ReactElement ControllerViewEventHandler) -- ^ The rendering function
                 -> ReactClass props
mkControllerView name (ReactStore store) buildNode = unsafePerformIO $ do
    renderCb <- syncCallback1 ThrowWouldBlock $ \arg -> do

        storeDataE <- js_RenderCbRetrieveState arg
        mdata <- derefExport storeDataE
        storeData <- maybe (error "Unable to load store state") return mdata

        propsE <- js_RenderCbRetrieveProps arg
        mprops <- derefExport propsE
        props <- maybe (error "Unable to load props") return mprops

        let node = fmap expandCtrlViewHandler $ buildNode storeData props
        (element, evtCallbacks) <- runWriterT $ renderNode node

        evtCallbacksRef <- toJSArg evtCallbacks
        js_RenderCbSetResults arg evtCallbacks element

    releaseCb <- syncCallback1 ThrowWouldBlock releaseCallback

    releaseProps <- syncCallback1 ThrowWouldBlock releaseExport

    ReactClass <$> js_createControllerView (toJSString name) store renderCb releaseCb releaseProps

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
                releaseProps(this.props);
            }, \
            componentWillReceiveProps: function() { \
                releaseProps(this.props); \
            }, \
            render: function() { \
                var arg = { \
                    state: this.state, \
                    getState: function() { return this.state; }, \
                    setState: this.setState, \
                    props: this.props, \
                    newCallbacks: [], \
                    result:0, \
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

-- | A stateful class event handler transforms events into store actions and a new state.
-- If the new state is nothing, no change is made to the state (which allows an optimization in that
-- we do not need to re-render the view).
type StatefulClassEventHandler state = state -> Event -> ([SomeStoreAction], Maybe state)

-- | Transform a stateful class event handler to a raw event handler
expandStatefulClassHandler :: (FromJSON state, ToJSON state)
                           => GetStateFn -> SetStateFn -> StatefulClassEventHandler state -> RawEventHandler
expandStatefulClassHandler getState setState handler evt = do
    state <- parseState <$> js_GetState getState
    let (actions, mNewState) = handler state evt
    case mNewState of
        Nothing -> return ()
        Just newState -> do
            newStateRef <- toJSRef_aeson newState
            js_SetState setState newStateRef
    mapM_ dispatchSomeAction actions

-- | A stateful class keeps track of internal state, but the definition of the rendering and
-- event handlers are pure functions.
--
-- TODO
-- >todoInput :: ReactClass String
-- >todoInput = mkStatefulClass "todoInput" "" $ \state props ->
-- >    let ct = if null state then props else state
-- >     in el "input" [evt "onChange" (\oldState evt -> ([], oldState ++ evt)]
mkStatefulClass :: (ToJSON state, FromJSON state, Typeable props)
            => String -- ^ A name for this class
            -> state -- ^ The initial state
            -> (state -> props -> ReactElement (PureClassEventHandler state)) -- ^ The rendering function
            -> ReactClass props
mkStatefulClass name initial render = mkClassHelper expandStatefulClassHandler name initial render'
    where
        render' state props = return $ render state props

-- | A class event handler can perform arbitrary IO, producing the new state.  If the new state is
-- nothing, no change is made to the state (which allows an optimization in that we do not need to
-- re-render the view).
type ClassEventHandler state = state -> Event -> IO (Maybe state)

-- | Transform a class event handler to a raw event handler.
expandClassHandler :: (FromJSON state, ToJSON state)
                   => GetStateFn -> SetStateFn -> ClassEventHandler state -> RawEventHandler
expandClassHandler getState setState handler evt = do
    state <- parseState <$> js_GetState getState
    mNewState <- handler state evt
    case mNewState of
        Nothing -> return ()
        Just newState -> do
            newStateRef <- toJSRef_aeson newState
            js_SetState setState newStateRef

-- | Create a class allowed to perform arbitrary IO during rendering and event handlers.
mkClass :: (ToJSON state, FromJSON state, Typeable props)
        => String -- ^ A name for this class
        -> state -- ^ The initial state
        -> (state -> props -> IO (ReactElement (ClassEventHandler state))) -- ^ The rendering function
        -> ReactClass props
mkClass = mkClassHelper expandClassHandler

-- | Helper function used for 'mkStatefulClass' and 'mkClass'
mkClassHelper :: (ToJSON state, FromJSON state, Typeable props)
              => (GetStateFn -> SetStateFn -> eventHandler -> RawEventHandler))
              -> String
              -> state
              -> (state -> props -> IO (ReactElement eventHandler))
              -> ReactClass props
mkClassHelper expandHandler name initial render = unsafePerformIO $ do

    initialRef <- toJSRef_aeson initial

    renderCb <- syncCallback1 ThrowWouldBlock $ \arg -> do

        stateRef <- js_RenderCbRetrieveState arg

        propsE <- js_RenderCbRetrieveProps arg
        mprops <- derefExport propsE
        props <- maybe (error "Unable to load props") return mprops

        getStateFn <- js_RenderCbRetrieveGetStateFn
        setStateFn <- js_RenderCbRetrieveSetStateFn
        let f = expandHandler getStateFn setStateFn
        node <- fmap f <$> render state props

        (element, evtCallbacks) <- runWriterT $ renderNode node

        evtCallbacksRef <- toJSArg evtCallbacks
        js_RenderCbSetResults arg evtCallbacks element

    releaseCb <- syncCallback1 ThrowWouldBlock releaseCallback

    releaseProps <- syncCallback1 ThrowWouldBlock releaseExport

    ReactClass <$> js_createClass (toJSString name) initialRef renderCb releaseCb releaseProps

-- | Parse the state from a JSRef.
parseState :: FromJSON state => JSRef state -> IO state
parseState stateRef = do
    mstate <- fromJSRef stateRef
    stateVal <- maybe (error "Unable to decode class state") return mstate
    state <- case fromJSON stateVal of
                Error err -> error $ "Unable to decode class state: " ++ err
                Success s -> return s

module React.Flux.Class (
    ReactClass_
  , ReactClassRef
  , ReactClass(..)
  , ComponentViewEventHandler
  , mkComponentView
  , PureClassEventHandler
  , mkPureClass
  , ClassEventHandler
  , mkClass

) where

-- | This type is for the return value of React.createClass.
data ReactClass_

-- | A JSRef to a javascript React class.
type ReactClassRef props = JSRef ReactClass_

-- | TODO
newtype ReactClass props = ReactClass (ReactClassRef_ props)

-- | The class render callback is a haskell function given to the javascript class
-- that is called every time the class is to be rendered.  The argument to this callback is
-- javascript object (since there are 4 arguments and the "GHCJS.Foreign.Callbacks" module
-- only creates callbacks with up to two arguments).  The properties of this object are:
--
-- * state :: Export state
-- * props :: Export props
-- * setState.  The setState function for this class.
-- * newCallbacks :: [Callback].  This array is set by Haskell, and contains all the event
--       callbacks created as part of the rendering.  These callbacks will be stored and
--       after the next render, these callbacks will be freed.
-- * elem  This is set by Haskell and contains the value that should be returned by the render
--         function back to React.
data RenderCbArgs

foriegn import javascript unsafe
    "$1.state"
    js_RenderCbRetrieveState :: JSRef RenderCbArgs -> IO (JSRef state)

foreign import javascript unsafe
    "$1.props"
    js_RenderCbRetrieveProps :: JSRef RenderCbArgs -> IO (Export props)

type SetStateJSFun = JSRef (JSRef state -> IO ())

foreign import javascript unsafe
    "$1.setState"
    js_RenderCbRetrieveSetState :: JSRef RenderCbArgs -> IO SetStateJSFun

foreign import javascript unsafe
    "$1($2)"
    js_SetState :: SetStateJSFun -> JSRef state -> IO ()

foreign import javascript unsafe
    "$1.newCallbacks = $2; $1.elem = $3;"
    js_RenderCbSetResults :: JSRef RenderCbArgs -> JSRef a -> JSRef b -> IO ()

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
    js_createComponentView :: JSString
                           -> ReactStoreRef storeData
                           -> (JSFun (JSRef RenderCbArgs -> IO ()))
                           -> (JSFun (Callback a -> IO ()))
                           -> (JSFun (Export a -> IO ())
                           -> IO (ReactClassRef props)

type ComponentViewEventHandler = Event -> [SomeStoreAction]

expandComponentHandler :: ComponentViewEventHandler -> RawEventHandler
expandComponentHandler handler evt = mapM_ dispatchSome $ handler evt
    where
        dispatchSome (SomeStoreAction store action) = dispatch store action

mkComponentView :: Typeable props
                => String
                -> ReactStore storeData
                -> (storeData -> props -> ReactElement ComponentViewEventHandler)
                -> ReactClass props
mkComponentView name (ReactStore store) buildNode = unsafePerformIO $ do
    renderCb <- syncCallback1 ThrowWouldBlock $ \arg -> do

        storeDataE <- js_RenderCbRetrieveState arg
        mdata <- derefExport storeDataE
        storeData <- maybe (error "Unable to load store state") return mdata

        propsE <- js_RenderCbRetrieveProps arg
        mprops <- derefExport propsE
        props <- maybe (error "Unable to load props") return mprops

        let node = fmap expandComponentHandler $ buildNode storeData props
        (element, evtCallbacks) <- runWriterT $ renderNode node

        evtCallbacksRef <- toJSArg evtCallbacks
        js_RenderCbSetResults arg evtCallbacks element

    releaseCb <- syncCallback1 ThrowWouldBlock releaseCallback

    releaseProps <- syncCallback1 ThrowWouldBlock releaseExport

    ReactClass <$> js_createComponentView (toJSString name) store renderCb releaseCb releaseProps

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

type PureClassEventHandler state = Event -> ([SomeStoreAction], state)

expandPureClassHandler :: PureClassEventHandler state -> ClassEventHandler state
expandPureClassHandler handler setState evt = do
    let (actions, newState) <- handler evt
    setState newState
    let dispatchSome (SomeStoreAction store action) = dispatch store action
    mapM_ dispatchSome actions

mkPureClass :: (ToJSON state, FromJSON state, Typeable props)
            => String
            -> state
            -> (state -> props -> ReactElement (PureClassEventHandler state))
            -> ReactClass props
mkPureClass name initial render = mkClass name initial render'
    where
        render' state props = fmap expandPureClassHandler $ render state props

type ClassEventHandler state = (state -> IO ()) -> Event -> IO ()

expandClassHandler :: ToJSON state => SetStateJSFun -> ClassEventHandler state -> RawEventHandler
expandClassHandler setStateJs handler evt = do
    let setState state = do newStateRef <- toJSRef_aeson newState
                            js_SetState setState newStateRef
    handler setState evt

mkClass :: (ToJSON state, FromJSON state, Typeable props)
        => String
        -> state
        -> (state -> props -> ReactElement (ClassEventHandler state))
        -> ReactClass props
mkClass name initial render = unsafePerformIO $ do

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

        let node = fmap expandClassHandler $ buildNode state props
        (element, evtCallbacks) <- runWriterT $ renderNode node

        evtCallbacksRef <- toJSArg evtCallbacks
        js_RenderCbSetResults arg evtCallbacks element

    releaseCb <- syncCallback1 ThrowWouldBlock releaseCallback

    releaseProps <- syncCallback1 ThrowWouldBlock releaseExport

    ReactClass <$> js_createComponentView (toJSString name) initialRef renderCb releaseCb releaseProps

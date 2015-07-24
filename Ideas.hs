module Ideas(
  -- * Store and Dispatcher
    ReactStore
  , StoreData(..)
  , SomeStoreAction(..)
  , mkStore
  , dispatch

  -- * Creating React Elements
  , ReactElement(..)

  -- * Component Views
  , ComponentViewEventHandler
  , mkComponentView

  -- * Views

  -- * Classes
  , ClassEventHandler
  , mkClass
  , ReactClass
) where

-- | Opaque type for the store object managed by javascript
data ReactStore_
-- | A JSRef to a store object
type ReactStoreRef storeData = JSRef ReactStore_

-- | TODO
newtype ReactStore storeData = ReactStore (ReactStoreRef storeData)

-- | TODO
class StoreData storeData where
    type StoreAction storeData
    transform :: StoreAction storeData -> storeData -> IO storeData

-- | Create a store, which is a javascript object with three properties:
--
-- * st: holds a value of type @Export storeData@ which is the current store
--
-- * views: an array of @setState@ functions for component views.  The component views
--     add and remove from this property directly inside their lifecycle methods.
--
-- * dispatch: a callback which dispatches an event to the store.  This is essentially
--   
foreign import javascript unsafe
    "{st:$1, views: [], dispatch:$2}"
    js_CreateStore :: StoreData storeData
                   => Export storeData
                   -> JSFun (Export (StoreAction storeData) -> Export storeData -> IO ())
                   -> IO (ReactStoreRef storeData)

foreign import javascript unsafe
    "(function(store, newSt) { \
        store.st = newSt; \
        store.views.map(function(f) { f(store.st); }; \
    })($1, $2)"
    js_UpdateStore :: StoreData storeData
                   => ReactStoreRef storeData
                   -> Export storeData
                   -> IO ()

foreign import javascript unsafe
    "(function(store, action) { \
        store.dispatch(action, store.st);
    })($1, $2)"
    js_DispatchStore :: StoreData storeData
                     => ReactStoreRef storeData
                     -> (Export (StoreAction storeData))
                     -> IO ()

mkStore :: (StoreData storeData, Typeable storeData, Typeable (StoreAction storeData)) => storeData -> IO (ReactStore storeData)
mkStore initial = unsafePerformIO $ do
    i <- export initial
    cb <- asyncCallback2 $ \actionE oldDataE -> do

        -- dereference the action, releasing the export
        maction <- derefExport actionE
        releaseExport actionE
        action <- maybe (error "Unable to load action") return maction

        -- dereference the old state
        mold <- derefExport oldDataE
        oldData <- maybe (error "Unable to load store state") return mold

        -- compute the new state, and send it to React
        newData <- transform action oldData
        newDataE <- export newData
        js_UpdateStore store newDataE

        -- release the export for the old state
        releaseExport storeDataE

    ReactStore <$> jsCreateStore i cb

dispatch :: (StoreData storeData, Typeable (StoreAction storeData))
         => ReactStore storeData -> StoreAction storeData -> IO ()
dispatch (ReactStore store) action = do
    -- export the action.  This action will be released as part of the dispatch callback above
    actionE <- export action
    js_DispatchStore store actionE

data SomeStoreAction = forall storeData. (StoreData storeData, Typeable (StoreAction storeData))
    => SomeStoreAction (ReactStore storeData) (StoreAction storeData)


-------------------------------------------------------------------------------------

data ReactClass_
type ReactClassRef props = JSRef ReactClass_
newtype ReactClass props = ReactClass (ReactClassRef_ props)

foreign import javascript unsafe
    "(function(name, store, renderCb, releaseCb, releaseProps) {
        React.createClass({ \
            displayName: name,
            getInitialState: function() { \
                return store.st; \
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
    js_createComponentView :: JSString
                           -> ReactStoreRef storeData
                           -> (JSFun (JSObject () -> IO ()))
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

        storeDataE <- getProp "state" arg
        mdata <- derefExport storeDataE
        storeData <- maybe (error "Unable to load store state") return mdata

        propsE <- getProp "props" arg
        mprops <- derefExport propsE
        props <- maybe (error "Unable to load props") return mprops

        let node = fmap expandComponentHandler $ buildNode storeData props
        (element, evtCallbacks) <- runWriterT $ renderNode node

        evtCallbacksRef <- toJSArg evtCallbacks
        setProp "newCallbacks" evtCallbacksRef arg
        setProp "result" result arg

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
                   -> JSFun (JSObject () -> IO ())
                   -> JSFun (Callback a -> IO ())
                   -> JSFun (Export a -> IO ())
                   -> IO (ReactClassRef props)

foreign import javascript unsafe
    "$1.setState($2)"
    js_SetState :: JSObject () -> JSRef state -> IO ()

type ClassEventHandler state = Event -> ([SomeStoreAction], state)

expandClassHandler :: ToJSON state => JSObject () -> ClassEventHandler state -> RawEventHandler
expandClassHandler args handler evt = do
    let (actions, newState) <- handler evt
    newStateRef <- toJSRef_aeson newState
    js_SetState args newStateRef
    let dispatchSome (SomeStoreAction store action) = dispatch store action
    mapM_ dispatchSome actions

mkClass :: (ToJSON state, FromJSON state, Typeable props)
        => String
        -> state
        -> (state -> props -> ReactElement (ClassEventHandler state))
        -> ReactClass props
mkClass name initial render = unsafePerformIO $ do

    initialRef <- toJSRef_aeson initial

    renderCb <- syncCallback1 ThrowWouldBlock $ \arg -> do

        stateRef <- getProp "state" arg
        mstate <- fromJSRef storeDataE
        stateVal <- maybe (error "Unable to decode class state") return mstate
        state <- case fromJSON stateVal of
                    Error err -> error $ "Unable to decode class state: " ++ err
                    Success s -> return s

        propsE <- getProp "props" arg
        mprops <- derefExport propsE
        props <- maybe (error "Unable to load props") return mprops

        let node = fmap expandClassHandler $ buildNode state props
        (element, evtCallbacks) <- runWriterT $ renderNode node

        evtCallbacksRef <- toJSArg evtCallbacks
        setProp "newCallbacks" evtCallbacksRef arg
        setProp "result" result arg

    releaseCb <- syncCallback1 ThrowWouldBlock releaseCallback

    releaseProps <- syncCallback1 ThrowWouldBlock releaseExport

    ReactClass <$> js_createComponentView (toJSString name) initialRef renderCb releaseCb releaseProps

{-
foreign import javascript unsafe
    "React.createClass({ \
        displayName: $1, \
        render: function() { var x = {r:0}; $2(this.props, x); return x.r; } \
    })"
    js_createView :: JSString
                  -> (JSFun (JSRef props -> JSObject () -> IO ()))
                  -> IO (ReactClassRef props)

mkView :: FromJSON props
       => String -> (props -> ReactRenderNode) -> ReactClass props
mkView name buildNode = unsafePerformIO $ do
    callback <- syncCallback2 ThrowWouldBlock $ \propsRef retObj -> do
        props <- parseProps propsRef
        result <- renderNode $ buildNode props
        setProp "r" result retObj
    ReactClass <$> js_createView (toJSString name) callback

mkClass :: (ToJSON state, FromJSON state, FromJSON props)
        => String
        -> IO state -- ^ initial state
        -> (state -> props -> IO (ReactRenderNode (Event -> (state -> IO ()) -> IO ())))
        -> ReactClass props

parseProps :: FromJSON props => JSRef props -> IO props
parseProps propsRef = do
        mprops <- fromJSRef propsRef
        propsVal <- maybe (error "Unable to decode props") return mprops
        props <- case fromJSON propsVal of
                    Error err -> error $ "Unable to decode props: " ++ err
                    Success p -> return p
-}

-------------------------------------------------------------------------------------

type RawEventHandler = Event -> IO ()

data ReactElement eventHandler
    = forall attrs. ToJSON attrs => ForeignElement
        { fName :: String
        , fAttrs :: attrs
        , fEvents :: [(String, eventHandler)]
        , fChildren :: [ReactElement eventHandler]
        }
    | forall props. Typeable props => ClassElement
        { ceClass :: ReactClass props
        , ceProps :: props
        , ceChildren :: [ReactElement eventHandler]
        }

instance Functor ReactElement where
    fmap f (ForeignElement n a e c) = ForeignElement n (f a) e (map (fmap f) c)
    fmap f (ClassElement n p c) = ClassElement n p (map (fmap f) c)

data ReactElement_
type ReactElementRef = JSRef ReactElement_

foreign import javascript unsafe
    "React.createElement($1, $2, $3)"
    js_ReactCreateElement :: JSRef a -- ^ will either be a string or a ReactClassRef
                          -> JSRef b -- ^ the raw properties
                          -> JSArray ReactElementRef
                          -> IO ReactElementRef

mkEvent :: JSObject () -> (String, RawEventHandler) -> WriterT [Callback] IO (JSObject ())
mkEvent obj (str, handler) = do
    cb <- lift $ asyncCallback1 handler
    tell [cb]
    setProp str cb obj
    return obj

renderNode :: ReactElement RawEventHandler -> WriterT [Callback] IO ReactElementRef
renderNode (f@(ForeignElement{})) = do
    props <- toJSRef_aeson $ fAttrs f
    foldM mkEvent props $ fEvents f
    childNodes <- mapM renderNode $ fChildren f
    js_ReactCreateElement (toJSString $ fName f) props (pToJSRef childNodes)

renderNode (ClassInstance { ceClass = ReactClass rc, ceProps = props, ceChildren = children }) = do
    childNodes <- mapM renderNode children
    propsE <- export props -- this will be released inside the lifetime events for the class
    lift $ js_ReactCreateElement rc propsE (pToJSRef childNodes)

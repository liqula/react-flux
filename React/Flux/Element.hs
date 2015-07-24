module React.Flux.Element where

data Event_
newtype Event = JSRef Event_

-- | Event handlers in a controller-view and a view transform events into actions.
type ViewEventHandler = Event -> [SomeStoreAction]

-- | A stateful class event handler transforms events into store actions and a new state.
-- If the new state is nothing, no change is made to the state (which allows an optimization in that
-- we do not need to re-render the view).
type StatefulViewEventHandler state = state -> Event -> ([SomeStoreAction], Maybe state)

-- | A class event handler can perform arbitrary IO, producing the new state.  If the new state is
-- nothing, no change is made to the state (which allows an optimization in that we do not need to
-- re-render the view).
type ClassEventHandler state = state -> Event -> IO (Maybe state)

-- | An internal event handler, used by the rendering function
type RawEventHandler = Event -> IO ()

data ReactElement eventHandler
    = forall attrs. ToJSON attrs => ForeignElement
        { fName :: String
        , fAttrs :: attrs
        , fEvents :: [(String, eventHandler)]
        , fChildren :: [ReactElement eventHandler]
        }
    | forall props key. (Typeable props, ToJSRef key) => ClassElement
        { ceClass :: ReactClass props
        , ceKey :: Maybe key
        -- TODO: ref?  ref support would need to tie into the Class too.
        , ceProps :: props
        , ceChildren :: [ReactElement eventHandler]
        }

instance Functor ReactElement where
    fmap f (ForeignElement n a e c) = ForeignElement n (f a) e (map (fmap f) c)
    fmap f (ClassElement n k p c) = ClassElement n k p (map (fmap f) c)

data ReactElement_
type ReactElementRef = JSRef ReactElement_

foreign import javascript unsafe
    "React.createElement($1, $2, $3)"
    js_ReactCreateElement :: JSRef a -- ^ will either be a string or a ReactClassRef
                          -> JSRef b -- ^ the raw properties
                          -> JSRef [ReactElementRef] -- ^ children
                          -> IO ReactElementRef

foreign import javascript unsafe
    "React.createElement($1, {hs:$2}, $3)"
    js_ReactCreateClass :: JSRef a -- ^ will either be a string or a ReactClassRef
                        -> JSRef props -- ^ the properties
                        -> JSRef [ReactElementRef] -- ^ children
                        -> IO ReactElementRef

foreign import javascript unsafe
    "React.createElement($1, {key: $2, hs:$3}, $4)"
    js_ReactCreateKeyedClass :: JSRef a -- ^ will either be a string or a ReactClassRef
                             -> JSRef key
                             -> JSRef props -- ^ the properties
                             -> JSRef [ReactElementRef] -- ^ children
                             -> IO ReactElementRef

data ReactElementArg_
type ReactElementArg = JSRef ReactElementArg_

foreign import javascript unsafe
    "$1[$2] = $3;"
    js_AddHandler :: ReactElementArg
                  -> JSString
                  -> (Callback (Event -> IO ()))
                  -> IO ()

mkEventHandler :: ReactElementArg -> (String, RawEventHandler) -> WriterT [Callback] IO ReactElementArg
mkEventHandler arg (str, handler) = do
    cb <- lift $ asyncCallback1 handler -- this will be released by the render function of the class.
    tell [cb]
    js_AddHandler arg str cb
    return arg

renderNode :: ReactElement RawEventHandler -> WriterT [Callback] IO ReactElementRef
renderNode (f@(ForeignElement{})) = do
    props <- toJSRef_aeson $ fAttrs f
    foldM mkEventHandler props $ fEvents f
    childNodes <- mapM renderNode $ fChildren f
    js_ReactCreateElement (toJSString $ fName f) props (pToJSRef childNodes)

renderNode (ClassInstance { ceClass = ReactClass rc, ceProps = props, ceKey = mkey, ceChildren = children }) = do
    childNodes <- mapM renderNode children
    propsE <- export props -- this will be released inside the lifetime events for the class
    lift $ case mkey of
        Just key -> do
            keyRef <- toJSRef key
            js_ReactCreateKeyedElement rc keyRef propsE (pToJSRef childNodes)
        Nothing -> js_ReactCreateClass rc propsE (pToJSRef childNodes)

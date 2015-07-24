module React.Flux.Element where

data ReactElement eventHandler
    = forall attrs. ToJSON attrs => ForeignElement
        { fName :: String
        , fAttrs :: attrs
        , fEvents :: [EventHandler eventHandler]
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
    fmap f (ForeignElement n a e c) = ForeignElement n a (map changeHandler e) (map (fmap f) c)
        where
            changeHandler (EventHandler name oldHandler) = EventHandler nane (f . oldHandler)
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

mkEventHandler :: ReactElementArg -> EventHandler (IO ()) -> WriterT [Callback] IO ReactElementArg
mkEventHandler arg (EventHandler str handler) = do
    -- this will be released by the render function of the class.
    cb <- lift $ asyncCallback1 $ \evtRef -> do
        mevtVal <- fromJSRef evtRef
        evtVal <- maybe (error "Unable to parse event as a javascript object") return mevtVal
        handler $ RawEvent evtRef evtVal

    tell [cb]

    js_AddHandler arg str cb
    return arg

renderNode :: ReactElement (IO ()) -> WriterT [Callback] IO ReactElementRef
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

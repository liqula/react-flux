module React.Flux.Element (
    ReactElement(..)
  , ReactElementM(..)
  , el
  , foreignClass
  , rclass
  , rclassWithKey
  , mkReactElementM
  , ReactElementRef_
  , ReactElemementRef
) where

data ReactElement eventHandler
    = ForeignElement
        { fName :: String
        , fAttrs :: Object
        , fHandlers :: [EventHandler eventHandler]
        , fChild :: ReactElement eventHandler
        }
    | forall props key. (Typeable props, ToJSRef key) => ClassElement
        { ceClass :: ReactClass props
        , ceKey :: Maybe key
        -- TODO: ref?  ref support would need to tie into the Class too.
        , ceProps :: props
        , ceChild :: ReactElement eventHandler
        }
    | Content String
    | Append (ReactElement eventHandler) (ReactElement eventHandler)
    | EmptyElement

instance Monoid (ReactElement eventHandler) where
    mempty = EmptyElement
    mappend x y = Append x y

instance Functor ReactElement where
    fmap f (ForeignElement n a e c) = ForeignElement n a (map changeHandler e) (map (fmap f) c)
        where
            changeHandler (EventHandler name oldHandler) = EventHandler nane (f . oldHandler)
    fmap f (ClassElement n k p c) = ClassElement n k p (map (fmap f) c)

newtype ReactElementM eventHandler a = ReactElementM { runReactElementM :: Writer (ReactElement eventHandler) a }
    deriving (Functor, Applicative, Monad, MonadPlus, Foldable, Traversable, Alternative)

instance (a ~ ()) => IsString (ReactElementM eventHandler a) where
    fromString s = ReactElementM (WriterT (Identity (Content s, ())))


el :: String -> [Pair] -> [EventHandler eventHandler] -> ReactElementM eventHandler a -> ReactElementM eventHandler a
el name attrs handlers (ReactElementM child) =
    let (a, childEl) <- runWriter child
     in ReactElementM (WriterT (Identity (ForeignElement name (object attrs) handlers childEl, a)))

foreignClass :: String -> [Pair] -> ReactElementM eventHandler a -> ReactElementM eventHandler a
foreignClass name attrs = el name attrs []

rclass :: Typeable props => ReactClass props -> props -> ReactElementM eventHandler a -> ReactElementM eventHandler a
rclass c p (ReactElementM child) =
    let (a, childEl) <- runWriter child
     in ReactElementM (WriterT (Identity (ClassElement c (Nothing :: Maybe ()) props childEl, a)))

rclassWithKey :: (Typeable props, ToJSRef key) => ReactClass props -> key -> props -> ReactElementM eventHandler a -> ReactElementM eventHandler a
rclassWithKey c key p (ReactElementM child) =
    let (a, childEl) <- runWriter child
     in ReactElementM (WriterT (Identity (ClassElement c (Just key) props childEl, a)))

---------------------------


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

js_ReactCreateContent :: String -> ReactElementRef
js_ReactCreateContent = castRef . toJSString

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

mkReactElem :: ReactElement eventHandler -> WriterT [Callback] IO [ReactElementRef]
mkReactElem EmptyElement = return []
mkReactElem (Append x y) = (++) <$> mkReactElem x <*> mkReactElem y
mkReactElem (Content s) = return [js_ReactCreateContent s]
mkReactElem (f@(ForeignElement{})) = do
    props <- lift $ toJSRef_aeson $ fAttrs f
    foldM_ mkEventHandler props $ fEvents f
    childNodes <- mkReactElem $ fChild f
    e <- lift $ js_ReactCreateElement (toJSString $ fName f) props (pToJSRef childNodes)
    return [e]
mkReactElem (ClassInstance { ceClass = ReactClass rc, ceProps = props, ceKey = mkey, ceChild = child }) = do
    childNodes <- mkReactElem child
    propsE <- lift $ export props -- this will be released inside the lifetime events for the class
    e <- lift $ case mkey of
        Just key -> do
            keyRef <- toJSRef key
            js_ReactCreateKeyedElement rc keyRef propsE (pToJSRef childNodes)
        Nothing -> js_ReactCreateClass rc propsE (pToJSRef childNodes)
    return [e]

mkReactElementM :: ReactElementM (IO ()) a -> IO (ReactElementRef, [Callback])
mkReactElementM e = runWriterT $ do
    let elem = execWriter $ runReactElementM e
    refs <- mkReactElem elem
    case refs of
        [] -> lift $ js_ReactCreateElement "div" jsNull jsNull
        [x] -> return x
        xs -> lift $ js_ReactCreateElement "div" jsNull (pToJSRef xs)

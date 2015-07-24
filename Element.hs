module React.Flux.Element where

data Event_
newtype Event = JSRef Event_
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

renderNode (ClassInstance { ceClass = ReactClass rc, ceProps = props, ceChildren = children }) = do
    childNodes <- mapM renderNode children
    propsE <- export props -- this will be released inside the lifetime events for the class
    lift $ js_ReactCreateElement rc propsE (pToJSRef childNodes)

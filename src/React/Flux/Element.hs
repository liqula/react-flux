module React.Flux.Element (
    ReactElement(..)
  , ReactElementM(..)
  , elementToM
  , el
  , foreignClass
  , ReactElementRef
  , ReactElement_
  , mkReactElementM
) where

import Data.String (IsString(..))
import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Typeable (Typeable)
import Control.Monad.Writer (Writer, runWriter, WriterT(..))
import Control.Monad.Identity (Identity(..))

import React.Flux.Events
import React.Flux.JsTypes

data ReactElement eventHandler
    = ForeignElement
        { fName :: String
        , fAttrs :: Value
        , fHandlers :: [EventHandler eventHandler]
        , fChild :: ReactElement eventHandler
        }
    | forall props key. (Typeable props, ToJSON key) => ClassElement
        {
#ifdef __GHCJS__
          ceClass :: ReactClassRef props
#else
          ceClass :: String
#endif
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
    fmap f (ForeignElement n a h c) = ForeignElement n a (map (fmap f) h) (fmap f c)
    fmap f (ClassElement n k p c) = ClassElement n k p (fmap f c)
    fmap f (Append a b) = Append (fmap f a) (fmap f b)
    fmap _ (Content s) = Content s
    fmap _ EmptyElement = EmptyElement

newtype ReactElementM eventHandler a = ReactElementM { runReactElementM :: Writer (ReactElement eventHandler) a }
    deriving (Functor, Applicative, Monad, Foldable)

elementToM :: a -> ReactElement eventHandler -> ReactElementM eventHandler a
elementToM a e = ReactElementM (WriterT (Identity (a, e)))

instance (a ~ ()) => Monoid (ReactElementM eventHandler a) where
    mempty = elementToM () EmptyElement
    mappend e1 e2 =
        let ((),e1') = runWriter $ runReactElementM e1
            ((),e2') = runWriter $ runReactElementM e2
         in elementToM () $ Append e1' e2'

instance (a ~ ()) => IsString (ReactElementM eventHandler a) where
    fromString s = elementToM () $ Content s

el :: String -> [Pair] -> [EventHandler eventHandler] -> ReactElementM eventHandler a -> ReactElementM eventHandler a
el name attrs handlers (ReactElementM child) =
    let (a, childEl) = runWriter child
     in elementToM a $ ForeignElement name (object attrs) handlers childEl

foreignClass :: String -> [Pair] -> ReactElementM eventHandler a -> ReactElementM eventHandler a
foreignClass name attrs = el name attrs []

----------------------------------------------------------------------------------------------------
-- mkReactElementM has two versions
----------------------------------------------------------------------------------------------------

-- | Execute a ReactElementM to create a javascript React element and a list of callbacks attached
-- to nodes within the element.

#ifdef __GHCJS__

mkReactElementM :: ReactElementM (IO ()) a -> IO (ReactElementRef, [Callback (IO a)])
mkReactElementM e = runWriterT $ do
    let elem = execWriter $ runReactElementM e
    refs <- mkReactElem elem
    case refs of
        [] -> lift $ js_ReactCreateElement "div" jsNull jsNull
        [x] -> return x
        xs -> lift $ js_ReactCreateElement "div" jsNull (pToJSRef xs)


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

#else

mkReactElementM :: ReactElementM (IO ()) a -> IO (ReactElementRef, [Callback (IO a)])
mkReactElementM _ = return ((), [])

#endif

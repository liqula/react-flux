-- | Internal module for React.Flux
--
-- Normally you should not need to use anything in this module.  This module is only needed if you have
-- complicated interaction with custom javascript rendering code that isn't covered by the
-- combinators in "React.Flux.DOM".  Actually, I cannot think of any case not covered by the
-- existing combinators, but I exported this module anyway just in case.
module React.Flux.Internal(
    ReactClassRef(..)
  , ReactElementRef(..)
  , HandlerArg(..)
  , PropertyOrHandler(..)
  , ReactElement(..)
  , ReactElementM(..)
  , el
  , elementToM
  , mkReactElement
) where

import Data.String (IsString(..))
import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Typeable (Typeable)
import Control.Monad.Writer
import Control.Monad.Identity (Identity(..))

#ifdef __GHCJS__
import GHCJS.Types (JSRef, castRef)
import GHCJS.Foreign.Callback (Callback, asyncCallback1)
import GHCJS.Foreign.Export (export)
import GHCJS.Marshal (toJSRef_aeson, fromJSRef)
import GHCJS.Marshal.Pure (pToJSRef)
import qualified Data.JSString as JSString
import qualified Data.JSString.Text as JSString
import qualified JavaScript.Array as JSArray
import qualified JavaScript.Object as JSObject
#else
type JSRef a = ()
type Callback a = JSRef a
type Export a = JSRef a
#endif

-- | This type is for the return value of @React.createClass@
newtype ReactClassRef props = ReactClassRef { reactClassRef :: JSRef () }

-- | This type is for the return value of @React.createElement@
newtype ReactElementRef = ReactElementRef { reactElementRef :: JSRef () }

-- | The first parameter of an event handler registered with React, and a decoded version of the argument.
data HandlerArg = HandlerArg
    { handlerArgRef :: JSRef ()
    , handlerArgVal :: Value
    }

-- | Either a property or an event handler.
--
-- The combination of all properties and event handlers are used to create the javascript object
-- passed as the second argument to @React.createElement@.  Properties are created with '(@=)' and
-- event handlers are created using the various functions below such as 'onKeyDown'.
data PropertyOrHandler handler =
   Property Pair
 | EventHandler
      { evtHandlerName :: String
      , evtHandler :: HandlerArg -> handler
      }

instance Functor PropertyOrHandler where
    fmap _ (Property p) = Property p
    fmap f (EventHandler name h) = EventHandler name (f . h)


-- | A React element is the result of the rendering function of a class.  It is a node or list
-- of nodes in a virtual DOM built by the rendering functions of classes, and React then reconciles
-- this virtual DOM with the browser DOM.  The 'ReactElement' is a monoid, so dispite its name can
-- represent more than one element as siblings.
--
-- A 'ReactElement' is parametrized by the type @eventHandler@, which is the type of the event
-- handlers that can be attached to DOM elements.  Event handlers are created by combinators in
-- "React.Flux.PropertiesAndEvents".
data ReactElement eventHandler
    = ForeignElement
        { fName :: Either String (ReactClassRef Object)
        , fProps :: [PropertyOrHandler eventHandler]
        , fChild :: ReactElement eventHandler
        }
    | forall props key. (Typeable props, ToJSON key) => ClassElement
        { ceClass :: ReactClassRef props
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
    fmap f (ForeignElement n p c) = ForeignElement n (map (fmap f) p) (fmap f c)
    fmap f (ClassElement n k p c) = ClassElement n k p (fmap f c)
    fmap f (Append a b) = Append (fmap f a) (fmap f b)
    fmap _ (Content s) = Content s
    fmap _ EmptyElement = EmptyElement

-- | A writer monad for 'ReactElement's which is used in the rendering function of all views.
--
-- @do@ notation or the 'Monoid' instance is used to sequence sibling elements.
-- Child elements are specified via function application; the combinator creating an element takes
-- the child element as a parameter. The @OverloadedStrings@ extension is used to create plain text.
--
-- >ul_ $ do li_ (b_ "Hello")
-- >         li_ "World"
-- >         li_ $
-- >             ul_ (li_ "Nested" <> li_ "List")
--
-- would build something like
--
-- ><ul>
-- >  <li><b>Hello</b><li>
-- >  <li>World</li>
-- >  <li><ul>
-- >    <li>Nested</li>
-- >    <li>List</li>
-- >  </ul></li>
-- ></ul>
--
-- The "React.Flux.DOM" module contains a large number of combinators for creating HTML elements.
-- 'React.Flux.rclass', 'React.Flux.rclassWithKey', and 'React.Flux.foreignClass' allow creating
-- React elements from classes.
newtype ReactElementM eventHandler a = ReactElementM { runReactElementM :: Writer (ReactElement eventHandler) a }
    deriving (Functor, Applicative, Monad, Foldable)

-- | Create a 'ReactElementM' containing a given 'ReactElement'.
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

-- | Create a React element.
el :: String -- ^ The element name (the first argument to @React.createElement@).
   -> [PropertyOrHandler eventHandler] -- ^ The properties to pass to the element (the second argument to @React.createElement@).
   -> ReactElementM eventHandler a -- ^ The child elements (the third argument to @React.createElement@).
   -> ReactElementM eventHandler a
el name attrs (ReactElementM child) =
    let (a, childEl) = runWriter child
     in elementToM a $ ForeignElement (Left name) attrs childEl

----------------------------------------------------------------------------------------------------
-- mkReactElement has two versions
----------------------------------------------------------------------------------------------------

-- | Execute a ReactElementM to create a javascript React element and a list of callbacks attached
-- to nodes within the element.  These callbacks will need to be released with 'releaseCallback'
-- once the class is re-rendered.
mkReactElement :: (eventHandler -> IO ())
               -> ReactElementM eventHandler ()
               -> IO (ReactElementRef, [Callback (JSRef Value -> IO ())])

#ifdef __GHCJS__

mkReactElement runHandler eM = runWriterT $ do
    let e = execWriter $ runReactElementM eM
    refs <- createElement $ fmap runHandler e
    case refs of
        [] -> lift $ js_ReactCreateElementNoChildren "div"
        [x] -> return x
        xs -> lift $ do
            emptyObj <- JSObject.create
            js_ReactCreateElementName "div" emptyObj (JSArray.fromList $ map reactElementRef xs)

foreign import javascript unsafe
    "React.createElement($1)"
    js_ReactCreateElementNoChildren :: JSString.JSString -> IO ReactElementRef

foreign import javascript unsafe
    "React.createElement($1, $2, $3)"
    js_ReactCreateElementName :: JSString.JSString -> JSObject.Object -> JSArray.JSArray -> IO ReactElementRef

foreign import javascript unsafe
    "React.createElement($1, $2, $3)"
    js_ReactCreateForeignElement :: ReactClassRef a -> JSObject.Object -> JSArray.JSArray -> IO ReactElementRef

foreign import javascript unsafe
    "React.createElement($1, {hs:$2}, $3)"
    js_ReactCreateClass :: ReactClassRef a -> JSRef props -> JSArray.JSArray -> IO ReactElementRef

foreign import javascript unsafe
    "React.createElement($1, {key: $2, hs:$3}, $4)"
    js_ReactCreateKeyedElement :: ReactClassRef a -> JSRef key -> JSRef props -> JSArray.JSArray -> IO ReactElementRef

js_ReactCreateContent :: String -> ReactElementRef
js_ReactCreateContent = ReactElementRef . castRef . pToJSRef

addPropOrHandlerToObj :: JSObject.Object -> PropertyOrHandler (IO ()) -> WriterT [Callback (JSRef Value -> IO ())] IO ()
addPropOrHandlerToObj obj (Property (n, v)) = lift $ do
    vRef <- toJSRef_aeson v
    JSObject.setProp (JSString.textToJSString n) vRef obj
addPropOrHandlerToObj obj (EventHandler str handler) = do
    -- this will be released by the render function of the class (jsbits/class.js)
    cb <- lift $ asyncCallback1 $ \evtRef -> do
        (mevtVal :: Maybe Value) <- fromJSRef evtRef
        evtVal <- maybe (error "Unable to parse event as a javascript object") return mevtVal
        handler $ HandlerArg (castRef evtRef) evtVal

    tell [cb]
    lift $ JSObject.setProp (JSString.pack str) (pToJSRef cb) obj

createElement :: ReactElement (IO ()) -> WriterT [Callback (JSRef Value -> IO ())] IO [ReactElementRef]
createElement EmptyElement = return []
createElement (Append x y) = (++) <$> createElement x <*> createElement y
createElement (Content s) = return [js_ReactCreateContent s]
createElement (f@(ForeignElement{})) = do
    obj <- lift $ JSObject.create
    mapM_ (addPropOrHandlerToObj obj) $ fProps f
    childNodes <- createElement $ fChild f
    let childArr = JSArray.fromList $ map reactElementRef childNodes
    e <- lift $ case fName f of
        Left s -> js_ReactCreateElementName (JSString.pack s) obj childArr
        Right ref -> js_ReactCreateForeignElement ref obj childArr
    return [e]
createElement (ClassElement { ceClass = rc, ceProps = props, ceKey = mkey, ceChild = child }) = do
    childNodes <- createElement child
    propsE <- lift $ export props -- this will be released inside the lifetime events for the class (jsbits/class.js)
    e <- lift $ case mkey of
        Just key -> do
            keyRef <- toJSRef_aeson key
            js_ReactCreateKeyedElement rc keyRef propsE (JSArray.fromList $ map reactElementRef childNodes)
        Nothing -> js_ReactCreateClass rc propsE (JSArray.fromList $ map reactElementRef childNodes)
    return [e]

#else

mkReactElement _ _ = return ((), [])

#endif

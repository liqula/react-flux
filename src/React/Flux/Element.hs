-- | Utility module for React Elements.
--
-- Normally you should not need to use anything in this module.  Instead, you can create DOM
-- elements from the functions in "React.Flux.DOM".  This module is only needed if you have
-- complicated interaction with custom javascript rendering code that isn't covered by the
-- combinators in "React.Flux.DOM".  Actually, I cannot think of any case not covered by the
-- existing combinators, but I exported this module anyway just in case.
module React.Flux.Element (
    ReactElement(..)
  , ReactElementM(..)
  , elementToM
  , el
  , foreignClass
  , ReactElementRef
  , ReactElement_
  , mkReactElement
) where

import Data.String (IsString(..))
import Data.Aeson
import Data.Typeable (Typeable)
import Control.Monad.Writer (Writer, runWriter, WriterT(..))
import Control.Monad.Identity (Identity(..))

import React.Flux.PropertiesAndEvents
import React.Flux.JsTypes

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

-- | Create a 'ReactElement' for a class defined in javascript.  For example, if you would like to
-- use <https://github.com/JedWatson/react-select react-select>, you could do so as follows:
--
-- >foreign import javascript unsafe
-- >    "require('react-select')"
-- >    js_GetReactSelectRef :: IO JSRef ()
-- >
-- >reactSelectRef :: JSRef ()
-- >reactSelectRef = unsafePerformIO $ js_GetReactSelectRef
-- >{-# NOINLINE reactSelectRef #-}
-- >
-- >select_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler a
-- >select_ props = foreignClass reactSelectRef props mempty
-- >
-- >onSelectChange :: FromJSON a
-- >               => (a -> handler) -- ^ receives the new value and performs an action.
-- >               -> PropertyOrHandler handler
-- >onSelectChange f = on "onChange" $ \handlerArg -> f $ parse handlerArg
-- >    where
-- >        parse (HandlerArg _ v) =
-- >            case fromJSON v of
-- >                Error err -> error $ "Unable to parse new value for select onChange: " ++ err
-- >                Success e -> e
--
-- This could then be used as part of a rendering function like so:
--
-- >div_ $ select_ [ "name" @= "form-field-name"
-- >               , "value" @= "one"
-- >               , "options" @= [ object [ "value" .= "one", "label" .= "One" ]
-- >                              , object [ "value" .= "two", "label" .= "Two" ]
-- >                              ]
-- >               , onSelectChange $ \newValue -> [AnAction newValue]
-- >               ]
--
-- Of course, in a real program the value and options would be built from the properties and/or
-- state of the view.
foreignClass :: JSRef () -- ^ The javascript reference to the class
             -> [PropertyOrHandler eventHandler] -- ^ properties and handlers to pass when creating an instance of this class.
             -> ReactElementM eventHandler a -- ^ The child element or elements
             -> ReactElementM eventHandler a
foreignClass name attrs (ReactElementM child) =
    let (a, childEl) = runWriter child
     in elementToM a $ ForeignElement (Right name) attrs childEl

----------------------------------------------------------------------------------------------------
-- mkReactElement has two versions
----------------------------------------------------------------------------------------------------

-- | Execute a ReactElementM to create a javascript React element and a list of callbacks attached
-- to nodes within the element.  These callbacks will need to be released with 'releaseCallback'
-- once the class is re-rendered.
mkReactElement :: ReactElementM (IO ()) a -> IO (ReactElementRef, [Callback (IO a)])

#ifdef __GHCJS__

mkReactElement e = runWriterT $ do
    let elem = execWriter $ runReactElementM e
    refs <- createElement elem
    case refs of
        [] -> lift $ js_ReactCreateElement "div" jsNull jsNull
        [x] -> return x
        xs -> lift $ js_ReactCreateElement "div" jsNull (pToJSRef xs)

foreign import javascript unsafe
    "React.createElement($1, $2, $3)"
    js_ReactCreateElement :: JSRef a -> JSRef b -> JSRef [ReactElementRef] -> IO ReactElementRef

foreign import javascript unsafe
    "React.createElement($1, {hs:$2}, $3)"
    js_ReactCreateClass :: JSRef a -> JSRef props -> JSRef [ReactElementRef] -> IO ReactElementRef

foreign import javascript unsafe
    "React.createElement($1, {key: $2, hs:$3}, $4)"
    js_ReactCreateKeyedClass :: JSRef a -> JSRef key -> JSRef props -> JSRef [ReactElementRef] -> IO ReactElementRef

js_ReactCreateContent :: String -> ReactElementRef
js_ReactCreateContent = castRef . toJSString

addPropOrHandlerToObj :: Object -> PropertyOrHandler (IO ()) -> WriterT [Callback] IO ()
addPropOrHandlerToObj obj (Property (n, v)) = do
    vRef <- toJSRef_aeson v
    setProp n vRef obj
addPropOrHandlerToObj obj (EventHandler str handler) = do
    -- this will be released by the render function of the class (jsbits/class.js)
    cb <- lift $ asyncCallback1 $ \evtRef -> do
        mevtVal <- fromJSRef evtRef
        evtVal <- maybe (error "Unable to parse event as a javascript object") return mevtVal
        handler $ RawEvent evtRef evtVal

    tell [cb]
    setProp str cb obj

createElement :: ReactElement eventHandler -> WriterT [Callback] IO [ReactElementRef]
createElement EmptyElement = return []
createElement (Append x y) = (++) <$> mkReactElem x <*> mkReactElem y
createElement (Content s) = return [js_ReactCreateContent s]
createElement (f@(ForeignElement{})) = do
    obj <- create
    mapM_ (addPropOrHandlerToObj obj) $ fProps f
    childNodes <- createElement $ fChild f
    e <- lift $ js_ReactCreateElement (either toJSString id $ fName f) props (pToJSRef childNodes)
    return [e]
createElement (ClassInstance { ceClass = ReactClass rc, ceProps = props, ceKey = mkey, ceChild = child }) = do
    childNodes <- createElement child
    propsE <- lift $ export props -- this will be released inside the lifetime events for the class (jsbits/class.js)
    e <- lift $ case mkey of
        Just key -> do
            keyRef <- toJSRef key
            js_ReactCreateKeyedElement rc keyRef propsE (pToJSRef childNodes)
        Nothing -> js_ReactCreateClass rc propsE (pToJSRef childNodes)
    return [e]

#else

mkReactElement _ = return ((), [])

#endif

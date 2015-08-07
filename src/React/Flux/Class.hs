-- | Internal module containing the class definitions
module React.Flux.Class (
    ReactClass(..)
  , mkControllerView
  , mkView
  , ViewEventHandler
  , mkStatefulView
  , StatefulViewEventHandler
  , mkClass
  , ClassEventHandler
  , rclass
  , rclassWithKey
  , foreignClass
) where

import Control.DeepSeq
import Control.Monad.Writer
import Data.Aeson
import Data.Typeable (Typeable)
import System.IO.Unsafe (unsafePerformIO)

import React.Flux.Store
import React.Flux.Internal
import React.Flux.Export

#ifdef __GHCJS__
import GHCJS.Types (JSRef, castRef, JSFun, JSString)
import GHCJS.Foreign (syncCallback1, toJSString, ForeignRetention(..))
import GHCJS.Marshal (toJSRef_aeson, fromJSRef, toJSRef)
#endif

type Callback a = JSFun a

-- | A React class is conceptually a (stateful) function from @props@ to a tree of elements.
-- The rendering function receives a value of type @props@ from its parent in the virtual DOM.
-- Additionally, the rendering function can depend on some internal state.  Based on the @props@ and
-- the internal state, the rendering function produces a virtual tree of elements which React then
-- reconciles with the browser DOM.
--
-- This module supports 3 kinds of pure classes and 1 unpure class, differing in how they manage the
-- internal state.
--
-- * Controller View, created by 'mkControllerView'.  A controller view provides the glue code
-- between a store and the view, and as such is a pure function taking as input the store data and
-- the properties and producing a tree of elements.  In addition, any event handlers attached to
-- elements can only produce actions.  No internal state is allowed.
--
-- * View.  A view is pure function from props to a tree of elements which does not maintain any
-- internal state.  It can eiter be modeled by
-- just a Haskell function without a 'ReactClass', or as a 'ReactClass' created by 'mkView'.  Using
-- the machinery of a 'ReactClass' is helpful because it allows React to more easily reconcile the
-- virtual DOM with the DOM and leads to faster rendering (as long as you use 'rclassWithKey'
-- when creating an instance of the view).
--
-- * Stateful View, created by 'mkStatefulView'.  A stateful view is a class which keeps track of
-- some internal state, but it is impossible for the state to leak outside of the class.  It
-- consists of a pure function taking as input the properties and current state and producing a tree
-- of elements.  Event handlers registered on elements can transform the state and produce actions,
-- but cannot perform any other @IO@.
--
-- The above classes are pure in the sense that this module enforces at the type level that no @IO@
-- can occur either during rendering or event handlers.  The only @IO@ that occurs is inside of the
-- 'transform' function on the store. Occasionally, you may need to create a class that performs
-- arbitrary IO either during rendering or event handlers, and this module does provide an
-- escape-hatch in 'mkClass'.
newtype ReactClass props = ReactClass { reactClass :: ReactClassRef props }

---------------------------------------------------------------------------------------------------
--- Two versions of mkControllerView
---------------------------------------------------------------------------------------------------

-- | Event handlers in a controller-view and a view transform events into actions, but are not
-- allowed to perform any 'IO'.
type ViewEventHandler = [SomeStoreAction]

-- | A controller view provides the glue between a 'ReactStore' and the DOM.
--
-- The controller-view registers with the given store.  Whenever the store is transformed, the
-- controller-view re-renders itself.  It is recommended to have one controller-view for each
-- significant section of the page.  Controller-views deeper in the page tree can cause complexity
-- because data is now flowing into the page in multiple possibly conflicting places.  You must
-- balance the gain of encapsulated components versus the complexity of multiple entry points for
-- data into the page.  Deeper components in the page can still be made re-usuable by using views or
-- stateful views.
--
-- Each instance of a controller-view also accepts properties of type @props@ from its parent.
-- Whenever the parent re-renders itself, the new properties will be passed down to the
-- controller-view causing it to re-render itself.
--
-- Events registered on controller-views just produce actions, which get dispatched to the
-- appropriate store which causes the store to transform itself, which eventually leads to the
-- controller-view re-rendering.  This one-way flow of data from actions to store to
-- controller-views is central to the flux design.
--
-- While the above re-rendering on any store data or property change is conceptually what occurs,
-- React uses a process of <https://facebook.github.io/react/docs/reconciliation.html reconciliation>
-- to speed up re-rendering.  The best way of taking advantage of re-rendering is to
-- use key properties with 'rclassWithKey'.
--
-- TODO
mkControllerView :: (StoreData storeData, Typeable props)
                 => String -- ^ A name for this class
                 -> ReactStore storeData -- ^ The store this controller view should attach to.
                 -> (storeData -> props -> ReactElementM ViewEventHandler ()) -- ^ The rendering function
                 -> ReactClass props

#ifdef __GHCJS__

mkControllerView name (ReactStore store _) buildNode = unsafePerformIO $ do
    let render sd props = return $ buildNode sd props
    renderCb <- mkRenderCallback parseExportStoreData runViewHandler render
    ReactClass <$> js_createControllerView (toJSString name) store renderCb

-- | Transform a controller view handler to a raw handler.
runViewHandler :: RenderCbArgs state props -> ViewEventHandler -> IO ()
runViewHandler _ handler = handler `deepseq` mapM_ dispatchSomeAction handler

#else

mkControllerView _ _ _ = ReactClass (ReactClassRef ())

#endif

{-# NOINLINE mkControllerView #-}

---------------------------------------------------------------------------------------------------
--- Two versions of mkView
---------------------------------------------------------------------------------------------------

-- | A view is a re-usable component of the page which does not track any state itself.
--
-- Each instance of a view accepts properties of type @props@ from its parent and re-renders itself
-- whenever the properties change.
--
-- One option to implement views is to just use a Haskell function taking the @props@ as input and
-- producing a 'ReactElementM'.  For small views, such a Haskell function is ideal.
-- Using a view class provides more than just a Haskell function when used with a key property with
-- 'rclassWithKey'.  The key property allows React to more easily reconcile the virtual DOM with the
-- browser DOM.

mkView :: Typeable props
       => String -- ^ A name for this class
       -> (props -> ReactElementM ViewEventHandler ()) -- ^ The rendering function
       -> ReactClass props

#ifdef __GHCJS__

mkView name buildNode = unsafePerformIO $ do
    let render () props = return $ buildNode props
    renderCb <- mkRenderCallback (const $ return ()) runViewHandler render
    ReactClass <$> js_createView (toJSString name) renderCb

#else

mkView _ _ = ReactClass (ReactClassRef ())

#endif

{-# NOINLINE mkView #-}

---------------------------------------------------------------------------------------------------
--- Two versions of mkStatefulView
---------------------------------------------------------------------------------------------------

-- | A stateful-view event handler produces a list of store actions and potentially a new state.  If
-- the new state is nothing, no change is made to the state (which allows an optimization in that we
-- do not need to re-render the view).
--
-- Changing the state causes a re-render which will cause a new event handler to be created.  If the
-- handler closes over the state passed into the rendering function, there is a race if multiple
-- events occur before React causes a re-render.  Therefore, the handler takes the current state as
-- input.  Your handlers therefore should ignore the state passed into the render function and
-- instead use the state passed directly to the handler.
type StatefulViewEventHandler state = state -> ([SomeStoreAction], Maybe state)

-- | A stateful view is a re-usable component of the page which keeps track of internal state.
--
-- The rendering function is a pure function of the state and the properties from the parent.  The
-- view will be re-rendered whenever the state or properties change.  The only way to
-- transform the internal state of the view is via an event handler, which can optionally produce
-- new state.
--
-- TODO
mkStatefulView :: (ToJSON state, FromJSON state, Typeable props)
               => String -- ^ A name for this class
               -> state -- ^ The initial state
               -> (state -> props -> ReactElementM (StatefulViewEventHandler state) ()) -- ^ The rendering function
               -> ReactClass props

#ifdef __GHCJS__

mkStatefulView name initial buildNode = unsafePerformIO $ do
    initialRef <- toJSRef_aeson initial
    let render state props = return $ buildNode state props
    renderCb <- mkRenderCallback parseJsonState runStateViewHandler render
    ReactClass <$> js_createClass (toJSString name) initialRef renderCb

-- | Transform a stateful class event handler to a raw event handler
runStateViewHandler :: (ToJSON state, FromJSON state)
                    => RenderCbArgs state props -> StatefulViewEventHandler state -> IO ()
runStateViewHandler args handler = do
    alterState <- js_RenderCbRetrieveAlterStateFns args
    st <- parseJsonState =<< js_GetState alterState

    let (actions, mNewState) = handler st

    case mNewState of
        Nothing -> return ()
        Just newState -> do
            newStateRef <- toJSRef_aeson newState
            js_SetState alterState newStateRef

    -- nothing above here should block, so the handler callback should still be running syncronous,
    -- so the deepseq of actions should still pick up the proper event object.
    actions `deepseq` mapM_ dispatchSomeAction actions

#else

mkStatefulView _ _ _ = ReactClass (ReactClassRef ())

#endif

{-# NOINLINE mkStatefulView #-}

---------------------------------------------------------------------------------------------------
--- Two versions of mkClass
---------------------------------------------------------------------------------------------------

-- | A class event handler can perform arbitrary IO, producing the new state.  If the new state is
-- nothing, no change is made to the state (which allows an optimization in that we do not need to
-- re-render the view).
--
-- Similar to 'StatefulViewEventHandler', the handler takes the current state as input to avoid a
-- race between changing the state and a re-render.
--
-- Also, if the 'IO' action blocks, the event object will become invalid because the haskell thread
-- will continue asyncronously and the handler registered with React will return. This causes React
-- to put the event object back into a pool to be reused.  Make sure you force any computation
-- depending on the event handler using 'deepseq' before it is possible for the IO action to block.
-- In particular, 'React.Flux.PropertiesAndEvents.preventDefault' and
-- 'React.Flux.PropertiesAndEvents.stopPropagation' must be forced before a possible block in the IO
-- action.
type ClassEventHandler state = state -> IO (Maybe state)

-- | Create a class allowed to perform arbitrary IO during rendering and event handlers.
mkClass :: (ToJSON state, FromJSON state, Typeable props)
        => String -- ^ A name for this class
        -> state -- ^ The initial state
        -> (state -> props -> IO (ReactElementM (ClassEventHandler state) ()))
        -- ^ The rendering function.  This function cannot block (for example, it cannot take an
        -- MVar).  If it does block, it will be aborted and a javascript execption will be thrown
        -- back to React causing React to abort the rendering.
        -> ReactClass props

#ifdef __GHCJS__

mkClass name initialState render = unsafePerformIO $ do
    initialRef <- toJSRef_aeson initialState
    renderCb <- mkRenderCallback parseJsonState runClassHandler render
    ReactClass <$> js_createClass (toJSString name) initialRef renderCb

-- | Transform a class event handler to a raw event handler.
runClassHandler :: (FromJSON state, ToJSON state)
                => RenderCbArgs state props -> ClassEventHandler state -> IO ()
runClassHandler args handler = do
    alterState <- js_RenderCbRetrieveAlterStateFns args
    st <- parseJsonState =<< js_GetState alterState
    mNewState <- handler st
    case mNewState of
        Nothing -> return ()
        Just newState -> do
            newStateRef <- toJSRef_aeson newState
            js_SetState alterState newStateRef
    return ()

#else

mkClass _ _ _ = ReactClass (ReactClassRef ())

#endif

{-# NOINLINE mkClass #-}

---------------------------------------------------------------------------------------------------
--- Various GHCJS only utilities
---------------------------------------------------------------------------------------------------

#ifdef __GHCJS__

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
--  In addition, for classes (not controller-views), there is one additional property @alterState@
--  which is used inside the event handlers.  @alterState@ has two properties, @setState@ and
--  @getState@.
newtype RenderCbArgs state props = RenderCbArgs (JSRef ())

foreign import javascript unsafe
    "$1.state"
    js_RenderCbRetrieveState :: RenderCbArgs state props -> IO (JSRef state)

foreign import javascript unsafe
    "$1.props"
    js_RenderCbRetrieveProps :: RenderCbArgs state props -> IO (Export props)

foreign import javascript unsafe
    "$1.newCallbacks = $2; $1.elem = $3;"
    js_RenderCbSetResults :: RenderCbArgs state props -> JSRef [Callback (JSRef Value -> IO ())] -> ReactElementRef -> IO ()

newtype AlterStateFns state = AlterStateFns (JSRef ())

foreign import javascript unsafe
    "$1.alterState"
    js_RenderCbRetrieveAlterStateFns :: RenderCbArgs state props -> IO (AlterStateFns state)

foreign import javascript unsafe
    "$1.setState($2)"
    js_SetState :: AlterStateFns state -> JSRef state -> IO ()

foreign import javascript unsafe
    "$1.getState()"
    js_GetState :: AlterStateFns state -> IO (JSRef state)

foreign import javascript unsafe
    "hsreact$mk_ctrl_view($1, $2, $3)"
    js_createControllerView :: JSString
                            -> ReactStoreRef storeData
                            -> Callback (JSRef () -> IO ())
                            -> IO (ReactClassRef props)

-- | Create a class with no state.
foreign import javascript unsafe
    "hsreact$mk_view($1, $2)"
    js_createView :: JSString
                  -> Callback (JSRef () -> IO ())
                  -> IO (ReactClassRef props)

-- | Create a class which tracks its own state.  Similar releasing needs to happen for callbacks and
-- properties as for controller views.
foreign import javascript unsafe
    "hsreact$mk_class($1, $2, $3)"
    js_createClass :: JSString
                   -> JSRef state
                   -> Callback (JSRef () -> IO ())
                   -> IO (ReactClassRef props)

mkRenderCallback :: Typeable props
                 => (JSRef state -> IO state) -- ^ parse state
                 -> (RenderCbArgs state props -> eventHandler -> IO ()) -- ^ execute event args
                 -> (state -> props -> IO (ReactElementM eventHandler ())) -- ^ renderer
                 -> IO (Callback (JSRef () -> IO ()))
mkRenderCallback parseState runHandler render = syncCallback1 AlwaysRetain False $ \argRef -> do
    let args = RenderCbArgs argRef
    stateRef <- js_RenderCbRetrieveState args
    state <- parseState stateRef

    propsE <- js_RenderCbRetrieveProps args
    mprops <- derefExport propsE
    props <- maybe (error "Unable to load props") return mprops

    node <- render state props

    (element, evtCallbacks) <- mkReactElement (runHandler args) node

    evtCallbacksRef <- toJSRef evtCallbacks
    js_RenderCbSetResults args evtCallbacksRef element

parseJsonState :: FromJSON state => JSRef state -> IO state
parseJsonState stateRef = do
    let valRef :: JSRef Value = castRef stateRef
    mval <- fromJSRef valRef
    case maybe (error "Unable to decode class state") fromJSON mval of
        Error err -> error $ "Unable to decode class state: " ++ err
        Success s -> return s

parseExportStoreData :: Typeable storeData => JSRef storeData -> IO storeData
parseExportStoreData storeDataRef = do
    mdata <- derefExport $ Export $ castRef storeDataRef
    maybe (error "Unable to load store state") return mdata

#endif


----------------------------------------------------------------------------------------------------
--- Element creation for classes
----------------------------------------------------------------------------------------------------

-- | Create an element from a class.  I suggest you make a combinator for each of your classes.  For
-- example,
--
-- TODO
rclass :: Typeable props => ReactClass props -- ^ the class
                         -> props -- ^ the properties to pass into the instance of this class
                         -> ReactElementM eventHandler a -- ^ The children of the element
                         -> ReactElementM eventHandler a
rclass rc props (ReactElementM child) =
    let (a, childEl) = runWriter child
     in elementToM a $ ClassElement (reactClass rc) (Nothing :: Maybe ()) props childEl

-- | Create an element from a class, and also pass in a key property for the instance.  Key
-- properties speed up the <https://facebook.github.io/react/docs/reconciliation.html reconciliation>
-- of the virtual DOM with the DOM.  The key does not need to be globally unqiue, it only needs to
-- be unique within the siblings of an element.
--
-- TODO
rclassWithKey :: (Typeable props, ToJSON key) => ReactClass props -- ^ the class
                                              -> key -- ^ A value unique within the siblings of this element
                                              -> props -- ^ The properties to pass to the class instance
                                              -> ReactElementM eventHandler a -- ^ The children of the class
                                              -> ReactElementM eventHandler a
rclassWithKey rc key props (ReactElementM child) =
    let (a, childEl) = runWriter child
     in elementToM a $ ClassElement (reactClass rc) (Just key) props childEl

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
foreignClass :: JSRef cl -- ^ The javascript reference to the class
             -> [PropertyOrHandler eventHandler] -- ^ properties and handlers to pass when creating an instance of this class.
             -> ReactElementM eventHandler a -- ^ The child element or elements
             -> ReactElementM eventHandler a
foreignClass name attrs (ReactElementM child) =
    let (a, childEl) = runWriter child
     in elementToM a $ ForeignElement (Right $ ReactClassRef $ castRef name) attrs childEl

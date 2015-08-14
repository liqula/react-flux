-- | React has <https://facebook.github.io/react/docs/working-with-the-browser.html lifecycle callbacks and refs>
-- that allows the class to interact with the browser DOM.  React obtains a
-- large performance boost from working with the virtual DOM instead of the browser DOM, so the use
-- of these lifecycle callbacks should be minimized or not used at all (in fact, the example TODO
-- app does not use them at all).  Quoting the
-- <https://facebook.github.io/react/docs/more-about-refs.html React documentation>, "If you have
-- not programmed several apps with React, your first inclination is usually going to be to try to
-- use refs to "make things happen" in your app. If this is the case, take a moment and think more
-- critically about where state should be owned in the component hierarchy. Often, it becomes clear
-- that the proper place to "own" that state is at a higher level in the hierarchy. Placing the
-- state there often eliminates any desire to use refs to "make things happen" – instead, the data
-- flow will usually accomplish your goal."
--
-- Additionally, the way GHCJS callbacks work causes potential problems with the lifecycle
-- callbacks: GHCJS callbacks can block and if that occurs they either abort with an error or
-- continue asyncronously.  Continuing asyncronously cannot work because by their nature these
-- lifecycle events are time-dependent, and by the time a Haskell thread resumes the element could
-- have disappeared.  Therefore, the lifecycle callbacks will abort with an error if one of them
-- blocks.  But because of the way GHCJS works, it is hard to control the possiblity of blocking
-- since a lazily computed value that you just happen to demand might block on a blackhole.
-- Therefore, this lifecycle view should only be used for simple things, such as scrolling to an
-- element when it is mounted.  This isn't a big restriction in my experience, since most of the
-- time you just use views and the rare time you need a lifecycle event, it is to do something
-- simple.
--
-- As an alternative to using this module and its resulting callback blocking complications, you can
-- consider writing the class in javascript\/typescript\/etc. and then using 'foreignClass' to call it
-- from Haskell.

module React.Flux.Lifecycle (
    defineLifecycleView
  , lifecycleConfig
  , LifecycleViewConfig(..)
  , LPropsAndState(..)
  , LDOM(..)
  , LSetStateFn
) where

import Control.Monad.Writer
import Data.Typeable (Typeable)
import System.IO.Unsafe (unsafePerformIO)

import React.Flux.Internal
import React.Flux.Export
import React.Flux.Views
import React.Flux.DOM (div_)

#ifdef __GHCJS__
import GHCJS.Types (JSRef, castRef)
import GHCJS.Foreign (syncCallback1, syncCallback2, toJSString, ForeignRetention(..), jsNull)
#endif

type HTMLElement = JSRef ()

-- | Actions to access the current properties and state.
data LPropsAndState props state = LPropsAndState
  { lGetProps :: IO props
  , lGetState :: IO state
  }

-- | Obtain the browser DOM element for either the component as a whole with 'lThis' or for various
-- nodes with a given <https://facebook.github.io/react/docs/more-about-refs.html ref> property with
-- 'lRef'.
data LDOM = LDOM
  { lThis :: IO HTMLElement
  , lRef :: String -> IO HTMLElement
  }

-- | Set the state of the class.
type LSetStateFn state = state -> IO ()

-- | The class rendering function, together with optional callbacks for the various lifecycle
-- events.  As mentioned above, care must be taken in each callback to write only IO that will not
-- block.
data LifecycleViewConfig props state = LifecycleViewConfig
  { lRender :: state -> props -> ReactElementM (StatefulViewEventHandler state) ()
  , lComponentWillMount :: Maybe (LPropsAndState props state -> LSetStateFn state -> IO ())
  , lComponentDidMount :: Maybe (LPropsAndState props state -> LDOM -> LSetStateFn state -> IO ())
  , lComponentWillReceiveProps :: Maybe (LPropsAndState props state -> LDOM -> LSetStateFn state -> props -> IO ())
  , lComponentWillUpdate :: Maybe (LPropsAndState props state -> LDOM -> props -> state -> IO ())
  , lComponentDidUpdate :: Maybe (LPropsAndState props state -> LDOM -> LSetStateFn state -> props -> state -> IO ())
  , lComponentWillUnmount :: Maybe (LPropsAndState props state -> LDOM -> IO ())
  }

-- | A default configuration, which does not specify any lifecycle events.  You should start with
-- this and override the functions you need.
lifecycleConfig :: LifecycleViewConfig props state
lifecycleConfig = LifecycleViewConfig
    { lRender = \_ _ -> div_ mempty
    , lComponentWillMount = Nothing
    , lComponentDidMount = Nothing
    , lComponentWillReceiveProps = Nothing
    , lComponentWillUpdate = Nothing
    , lComponentDidUpdate = Nothing
    , lComponentWillUnmount = Nothing
    }

-- | Create a lifecycle view from the given configuration.
--
-- >myView :: ReactView String
-- >myVew = defineLifecycleView "my view" (10 :: Int) lifecycleConfig
-- >            { lRender = \state props -> ...
-- >            , lComponentWillMount = \propsAndState setStateFn -> ...
-- >            }
defineLifecycleView :: (Typeable props, Typeable state)
              => String -> state -> LifecycleViewConfig props state -> ReactView props

#ifdef __GHCJS__

defineLifecycleView name initialState cfg = unsafePerformIO $ do
    initialRef <- export initialState

    let render state props = return $ lRender cfg state props
    renderCb <- mkRenderCallback (js_ReactGetState >=> parseExport) runStateViewHandler render

    let dom this = LDOM { lThis = js_ReactFindDOMNode this
                        , lRef = \r -> js_ReactGetRef this $ toJSString r
                        }

        setStateFn this s = export s >>= js_ReactUpdateAndReleaseState this

    willMountCb <- mkLCallback1 (lComponentWillMount cfg) $ \f this ->
        f (setStateFn this)

    didMountCb <- mkLCallback1 (lComponentDidMount cfg) $ \f this ->
        f (dom this) (setStateFn this)

    willRecvPropsCb <- mkLCallback2 (lComponentWillReceiveProps cfg) $ \f this newPropsE -> do
        newProps <- parseExport $ Export $ castRef newPropsE
        f (dom this) (setStateFn this) newProps

    willUpdateCb <- mkLCallback2 (lComponentWillUpdate cfg) $ \f this argRef -> do
        let arg = ReactThis argRef
        nextProps <- js_ReactGetProps arg >>= parseExport
        nextState <- js_ReactGetState arg >>= parseExport
        f (dom this) nextProps nextState

    didUpdateCb <- mkLCallback2 (lComponentDidUpdate cfg) $ \f this argRef -> do
        let arg = ReactThis argRef
        oldProps <- js_ReactGetProps arg >>= parseExport
        oldState <- js_ReactGetState arg >>= parseExport
        f (dom this) (setStateFn this) oldProps oldState

    willUnmountCb <- mkLCallback1 (lComponentWillUnmount cfg) $ \f this ->
        f (dom this)

    ReactView <$> js_makeLifecycleView (toJSString name) initialRef
                    renderCb willMountCb didMountCb willRecvPropsCb willUpdateCb didUpdateCb willUnmountCb

mkLCallback1 :: (Typeable props, Typeable state)
             => Maybe (LPropsAndState props state -> f)
             -> (f -> ReactThis state props -> IO ())
             -> IO (Callback (JSRef () -> IO ()))
mkLCallback1 Nothing _ = return jsNull
mkLCallback1 (Just f) c = syncCallback1 AlwaysRetain False $ \thisRef -> do
    let this = ReactThis thisRef
        ps = LPropsAndState { lGetProps = js_ReactGetProps this >>= parseExport
                            , lGetState = js_ReactGetState this >>= parseExport
                            }
    c (f ps) this

mkLCallback2 :: (Typeable props, Typeable state)
             => Maybe (LPropsAndState props state -> f)
             -> (f -> ReactThis state props -> JSRef a -> IO ())
             -> IO (Callback (JSRef () -> JSRef a -> IO ()))
mkLCallback2 Nothing _ = return jsNull
mkLCallback2 (Just f) c = syncCallback2 AlwaysRetain False $ \thisRef argRef -> do
    let this = ReactThis thisRef
        ps = LPropsAndState { lGetProps = js_ReactGetProps this >>= parseExport
                            , lGetState = js_ReactGetState this >>= parseExport
                            }
    c (f ps) this argRef

#else

lifecycleView _ _ _ = ReactView ()

#endif


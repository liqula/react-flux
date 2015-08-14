-- | React have lifecycle callbacks that allows the class to interact with the browser DOM.  React
-- obtains a large performance boost from working with the virtual DOM instead of the browser DOM,
-- so the use of these lifecycle callbacks should be minimized or not used at all (in fact, the
-- example TODO app does not use them at all).  Additionally, the way GHCJS callbacks work causes
-- potential problems with the lifecycle callbacks: GHCJS callbacks can block and if that occurs
-- they either abort with an error or continue asyncronously.  Continuing asyncronously cannot work
-- because by their nature these lifecycle events are time-dependent, and by the time a Haskell
-- thread resumes the element could have disappeared.  Therefore, the lifecycle callbacks will abort
-- with an error if one of them blocks.  But because of the way GHCJS works, it is hard to control
-- the possiblity of blocking since a lazily computed value that you just happen to demand might
-- block on a blackhole.  Therefore, this lifecycle view should only be used for simple things, such
-- as scrolling to an element when it is mounted.  This isn't a big restriction in my experience,
-- since most of the time you just use views and the rare time you need a lifecycle event, it is to
-- do something simple.
--
-- As an alternative to using this module and its resulting callback blocking complications, you can
-- consider writing the class in javascript/typescript/etc. and then using 'foreignClass' to call it
-- from Haskell.
module React.Flux.Lifecycle (
    lifecycleView
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

data LPropsAndState props state = LPropsAndState
  { lGetProps :: IO props
  , lGetState :: IO state
  }

data LDOM = LDOM
  { lThis :: IO HTMLElement
  , lRef :: String -> IO HTMLElement
  }

type LSetStateFn state = state -> IO ()

data LifecycleViewConfig props state = LifecycleViewConfig
  { lRender :: state -> props -> ReactElementM (StatefulViewEventHandler state) ()
  , lComponentWillMount :: Maybe (LPropsAndState props state -> LSetStateFn state -> IO ())
  , lComponentDidMount :: Maybe (LPropsAndState props state -> LDOM -> LSetStateFn state -> IO ())
  , lComponentWillReceiveProps :: Maybe (LPropsAndState props state -> LDOM -> LSetStateFn state -> props -> IO ())
  , lComponentWillUpdate :: Maybe (LPropsAndState props state -> LDOM -> props -> state -> IO ())
  , lComponentDidUpdate :: Maybe (LPropsAndState props state -> LDOM -> LSetStateFn state -> props -> state -> IO ())
  , lComponentWillUnmount :: Maybe (LPropsAndState props state -> LDOM -> IO ())
  }

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

lifecycleView :: (Typeable props, Typeable state)
              => String -> state -> LifecycleViewConfig props state -> ReactView props

#ifdef __GHCJS__

lifecycleView name initialState cfg = unsafePerformIO $ do
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


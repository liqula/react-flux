-- | Bindings for the <https://facebook.github.io/react/docs/addons.html React addons> that make
-- sense to use from Haskell.  At the moment, that is only the
-- <https://facebook.github.io/react/docs/animation.html animation> and
-- <https://facebook.github.io/react/docs/perf.html performance tools>.
module React.Flux.Addons.React (
  -- * Animation
    cssTransitionGroup
  , CSSTransitionProps(..)
  , defaultTransitionProps

  -- * Perf
  , PerfAction(..)
  , PerfPrint(..)
  , perfToggleButton_
  , perfA
) where

import Control.DeepSeq (NFData)
import Control.Monad (forM_)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import React.Flux

#ifdef __GHCJS__
import GHCJS.Types (JSRef, JSString)
#endif

-- | The properties for the CSS Transition Group.
data CSSTransitionProps = CSSTransitionProps
    { transitionName :: String
    , transitionAppear :: Bool
    , transitionEnter :: Bool
    , transitionLeave :: Bool
    } deriving (Typeable)

-- | Default properties for CSS Transition Group, using \"react-transition\" as the transition name.
defaultTransitionProps :: CSSTransitionProps
defaultTransitionProps = CSSTransitionProps
    { transitionName = "react-transition"
    , transitionAppear = False
    , transitionEnter = True
    , transitionLeave = True
    }

-- | The <https://facebook.github.io/react/docs/animation.html ReactCSSTransitionGroup> element.  At
-- the moment, only the high-level API is supported.
cssTransitionGroup :: CSSTransitionProps -> ReactElementM eventHandler a -> ReactElementM eventHandler a

#ifdef __GHCJS__
cssTransitionGroup p children = foreignClass js_CSSTransitionGroup props children
    where
        props = [ "transitionName" @= transitionName p
                , "transitionAppear" @= transitionAppear p
                , "transitionEnter" @= transitionEnter p
                , "transitionLeave" @= transitionLeave p
                ]

foreign import javascript unsafe
    "React['addons']['CSSTransitionGroup']"
    js_CSSTransitionGroup :: JSRef ()

#else
cssTransitionGroup _ x = x
#endif

--------------------------------------------------------------------------------
-- Perf
--------------------------------------------------------------------------------

data PerfStoreData = PerfStoreData { perfIsActive :: Bool }

-- | What to print after stopping performance measurement.  See
-- <https://facebook.github.io/react/docs/perf.html> for documentation.
data PerfPrint = PerfPrintInclusive
               | PerfPrintExclusive
               | PerfPrintWasted
               | PerfPrintDOM
    deriving (Show, Eq, Generic)

instance NFData PerfPrint

-- | An action to start or stop performance measurement.  For details, see
-- <https://facebook.github.io/react/docs/perf.html>.
data PerfAction = PerfStart
                | PerfStopAndPrint [PerfPrint]
    deriving (Show, Eq, Generic)

instance NFData PerfAction

instance StoreData PerfStoreData where
    type StoreAction PerfStoreData = PerfAction

    transform PerfStart _ = do
        js_perf "start"
        return $ PerfStoreData True

    transform (PerfStopAndPrint toPrint) _ = do
        js_perf "stop"
        forM_ toPrint $ \action -> do
            js_perf $ case action of
                PerfPrintInclusive -> "printInclusive"
                PerfPrintExclusive -> "printExclusive"
                PerfPrintWasted -> "printWasted"
                PerfPrintDOM -> "printDOM"
        return $ PerfStoreData False

perfStore :: ReactStore PerfStoreData
perfStore = mkStore $ PerfStoreData False

-- | Convert a performance action into a store action.   Use this if you are not using
-- 'perfToggleButton_'.
perfA :: PerfAction -> SomeStoreAction
perfA = SomeStoreAction perfStore

-- | The performance toggle button view
perfToggleButton :: ReactView [PerfPrint]
perfToggleButton = defineControllerView "perf toggle button" perfStore $ \sData toPrint ->
    button_ [ onClick $ \_ _ ->
                if perfIsActive sData
                    then [perfA $ PerfStopAndPrint toPrint]
                    else [perfA PerfStart]
            ] $
        if perfIsActive sData then "Stop perf measurement" else "Start perf measurement"

-- | A button which when clicked toggles the performance measurement on and off.  When the
-- measurement is stopped, the given measurements are printed.  If you want more control over the
-- performance tools, you can use 'perfA' directly from your own event handlers.
perfToggleButton_ :: [PerfPrint] -> ReactElementM handler ()
perfToggleButton_ toPrint = view perfToggleButton toPrint mempty

#ifdef __GHCJS__

foreign import javascript unsafe
    "React['addons']['Perf'][$1]()"
    js_perf :: JSString -> IO ()

#else

js_perf :: String -> IO ()
js_perf _ = return ()

#endif

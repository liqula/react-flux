-- | Bindings for the <https://facebook.github.io/react/docs/addons.html React addons> that make
-- sense to use from Haskell.  At the moment, that is only the
-- <https://facebook.github.io/react/docs/animation.html animation> and
-- <https://facebook.github.io/react/docs/perf.html performance tools>.
module React.Flux.Addons (
  -- * Animation
    cssTransitionGroup
  , CSSTransitionProps(..)
  , defaultTransitionProps

  --- * Perf
  , perfStart
  , perfStop
  , perfPrintInclusive
  , perfPrintExclusive
  , perfPrintWasted
  , perfPrintDOM
) where

import Data.Typeable (Typeable)

import React.Flux.Internal
import React.Flux.Views
import React.Flux.PropertiesAndEvents

#ifdef __GHCJS__
import GHCJS.Types (JSRef)
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

#ifdef __GHCJS__

foreign import javascript unsafe
    "React['addons']['Perf']['start']()"
    perfStart :: IO ()

foreign import javascript unsafe
    "React['addons']['Perf']['stop']()"
    perfStop :: IO ()

foreign import javascript unsafe
    "React['addons']['Perf']['printInclusive']()"
    perfPrintInclusive :: IO ()

foreign import javascript unsafe
    "React['addons']['Perf']['printExclusive']()"
    perfPrintExclusive :: IO ()

foreign import javascript unsafe
    "React['addons']['Perf']['printWasted']()"
    perfPrintWasted :: IO ()

foreign import javascript unsafe
    "React['addons']['Perf']['printDOM']()"
    perfPrintDOM :: IO ()

#else
perfStart, perfStop, perfPrintInclusive, perfPrintExclusive, perfPrintWasted, perfPrintDOM :: IO ()
perfStart = return ()
perfStop = return ()
perfPrintInclusive = return ()
perfPrintExclusive = return ()
perfPrintWasted = return ()
perfPrintDOM = return ()
#endif

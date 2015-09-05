-- | Bindings to <http://react-bootstrap.github.io/ React-Bootstrap>.  To use this
-- binding, include the browser-global version of @react-bootstrap.js@ so that @window.ReactBootstrap@
-- is defined.  You can then use 'bootstrap_' inside your rendering functions.
module React.Flux.Addons.Bootstrap (
    bootstrap_
) where

import React.Flux

#ifdef __GHCJS__
import GHCJS.Types (JSRef, JSString)
import React.Flux.Internal (toJSString)
#endif

-- | A bootstrap <http://react-bootstrap.github.io/components.html component>.  For example,
--
-- >bootstrap_ "Alert" [ "bsStyle" $= "danger"
-- >                   , callback "onDismiss" (const $ dispatch CloseAlert)
-- >                   ] $
-- >    p_ "Hello, World!"
bootstrap_ :: String
           -- ^ The component name.   Uses @window['ReactBootstrap'][name]@ to find the class, so
           -- the name can be anything exported to the @window.ReactBoostrap@ object.
           -> [PropertyOrHandler eventHandler]
           -- ^ Properties and callbacks to pass to the ReactBootstrap class.  You can use 'callback'
           -- to create function properties.
           -> ReactElementM eventHandler a -- ^ The child or children of the component.
           -> ReactElementM eventHandler a

#ifdef __GHCJS__

bootstrap_ n = foreignClass (js_ReactBootstrap $ toJSString n)

foreign import javascript unsafe
    "window['ReactBootstrap'][$1]"
    js_ReactBootstrap :: JSString -> JSRef ()

#else

bootstrap_ _ _ x = x

#endif

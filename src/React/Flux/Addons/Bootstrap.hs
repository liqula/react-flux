-- | Bindings to <http://react-bootstrap.github.io/ React-Bootstrap>.  To use this
-- binding, include the browser-global version of @react-bootstrap.js@ so that @window.ReactBootstrap@
-- is defined.  You can then use 'bootstrap_' inside your rendering functions.
module React.Flux.Addons.Bootstrap (
    bootstrap_
  , bdiv_
) where

import React.Flux
import Data.Text (Text)

#ifdef __GHCJS__
import GHCJS.Types (JSRef, JSString)
import React.Flux.Internal (toJSString)
#endif

-- | A bootstrap <http://react-bootstrap.github.io/components.html component>.  For example,
--
-- >bootstrap_ "Alert" [ "bsStyle" $= "danger"
-- >                   , callback "onDismiss" $ dispatch CloseAlert
-- >                   ] $
-- >    p_ "Hello, World!"
-- >
-- >bootstrap_ "Nav" [ "activeKey" @= (1 :: Int)
-- >                 , callback "onSelect" $ \(i :: Int) -> dispatch $ TabChange i
-- >                 ] $ do
-- >    bootstrap_ "NavItem" ["eventKey" @= (1 :: Int)] "Item 1"
-- >    bootstrap_ "NavItem" ["eventKey" @= (2 :: Int)] "Item 2"
-- >    bootstrap_ "NavItem" ["eventKey" @= (3 :: Int)] "Item 3"
bootstrap_ :: String
           -- ^ The component name.   Uses @window[\'ReactBootstrap\'][name]@ to find the class, so
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
    js_ReactBootstrap :: JSString -> JSRef

#else

bootstrap_ _ _ x = x

#endif

-- | A 'div_' that lets you specify only a list of class names, since a large majority of bootstrap
-- markup is adding class names to divs.  Multiple classes are separated by spaces.  If you need to
-- set class names based on computed booleans, use 'classNames' on a normal 'div_' instead.
--
-- >bdiv_ "row" $
-- >    bdiv_ "col-md-2 hidden-xs hidden-sm" $
-- >        bdiv_ "list-group" $
-- >            forM_ someList $ \item ->
-- >                a_ ["className" $= "list-group-item"] ...
bdiv_ :: Text -> ReactElementM eventHandler a -> ReactElementM eventHandler a
bdiv_ c = div_ ["className" @= c]

-- | At some point this should be replaced by GHCJS.Foreign.Export, but GHCJS.Foreign.Export
-- currently causes a bug in the Todo example application to appear.
module React.Flux.Export where

#ifdef __GHCJS__

import Data.Typeable (Typeable)
import Unsafe.Coerce

import GHCJS.Types

newtype Export a = Export JSRef

foreign import javascript unsafe
    "hsreact$export($1)"
    js_export :: Double -> IO (Export a)

export :: Typeable a => a -> IO (Export a)
export = js_export . unsafeCoerce

foreign import javascript unsafe
    "hsreact$derefExport($1)"
    js_deref :: Export a -> IO Double

derefExport :: Typeable a => Export a -> IO (Maybe a)
derefExport e = (Just . unsafeCoerce) <$> js_deref e

#endif

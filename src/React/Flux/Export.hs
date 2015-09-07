-- | Replace this with Export from improved-base branch of ghcjs-base once
-- the improved-base branch becomes the default
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

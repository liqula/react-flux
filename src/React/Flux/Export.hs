-- | Replace this with Export from improved-base branch of ghcjs-base once
-- the improved-base branch becomes the default
module React.Flux.Export(
    Export(..)
  , export
  , derefExport
) where

import Data.Typeable (Typeable)
import GHCJS.Types
import Unsafe.Coerce

newtype Export a = Export (JSRef ())

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

{-# LANGUAGE UndecidableInstances, AllowAmbiguousTypes, TypeApplications, BangPatterns, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module React.Flux.ForeignEq
  ( AllEq
  , singleEq
  , allEq
  ) where

import Data.Typeable
import GHCJS.Foreign.Callback
import GHCJS.Marshal (ToJSVal(..))
import GHCJS.Types (JSVal)

import React.Flux.Export


-- this is based on
-- https://bitbucket.org/wuzzeb/react-flux/pull-requests/9/use-eq-instead-of-comparing-generated-js/,
-- and it may be obsolete by now (@wuzzeb has reworked props and state equality between 2016-09-01
-- and 2017-03-30).
--
-- TODO: figure out whether we still need this, ever.  i don't think it does any harm semantically.
--
-- TODO: hsreact$mk_new_ctrl_view doesn't use haskell 'Eq' on states.  it just takes the comparison
-- function and ignores it.


type ForeignEq = JSVal -> JSVal -> IO JSVal
type ForeignEq_ = JSVal -> JSVal -> IO Bool

singleEq :: forall (t :: *). (Typeable t, Eq t) => Proxy t -> IO (Callback ForeignEq)
singleEq proxy = syncCallback2' (\jsa jsb -> toJSVal =<< singleEq_ proxy jsa jsb)

singleEq_ :: forall (t :: *). (Typeable t, Eq t) => Proxy t -> ForeignEq_
singleEq_ Proxy jsa jsb = do
  a <- parseExport $ Export jsa :: IO t
  b <- parseExport $ Export jsb :: IO t
  pure $ a == b

{-
-- from the discussion in
-- https://bitbucket.org/wuzzeb/react-flux/pull-requests/9/use-eq-instead-of-comparing-generated-js/:
-- this *may* help improving performance.
eqq_ :: Eq a => a -> a -> IO Bool
eqq_ !x !y = do
   sx <- makeStableName x
   sy <- makeStableName y
   return $ eqStableName sx sy || x == y
-}

allEq :: AllEq t => Proxy t -> IO (Callback ForeignEq)
allEq proxy = syncCallback2' (\jsa jsb -> toJSVal =<< allEq_ proxy 0 jsa jsb)

class AllEq t where
  allEq_ :: Proxy t -> Int -> ForeignEq_

instance AllEq '[] where
  allEq_ Proxy _ _ _ = pure True

instance (Typeable t, Eq t, AllEq ts) => AllEq (t ': ts) where
  allEq_ Proxy i jsas jsbs = do
    let jsa = js_findFromArray i jsas
        jsb = js_findFromArray i jsbs
    (&&) <$> (singleEq_ (Proxy :: Proxy t)  jsa jsb)
         <*> (allEq_    (Proxy :: Proxy ts) (i + 1) jsas jsbs)

-- | (This is similar to findFromState, but less specific, and more "pure".  not sure if we can merge
-- the two?)
foreign import javascript unsafe
  "($2.hs)[$1]"
  js_findFromArray :: Int -> JSVal -> JSVal

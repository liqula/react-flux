module React.Flux.Test (
    renderControllerView
  , renderView
  , renderStatefulView
  , renderClass
) where

import Control.Monad.Writer (execWriter)
import Data.Aeson
import Data.Typeable (Typeable, cast)
import React.Flux
import React.Flux.Class

runReact :: ReactElementM handler () -> ReactElement handler
runReact (ReactElementM w) = execWriter w

renderControllerView :: Typeable storeData => ReactClass props -> storeData -> props -> ReactElement ViewEventHandler
renderControllerView (TestReactControllerView _ f) storeData props =
    case cast storeData of
        Just storeData' -> runReact $ f storeData' props
        Nothing -> error "storeData passed to renderComponentView does not match the storeData in the controller view"
renderControllerView _ _ _ = error "The ReactClass passed to renderControllerView was not built by mkControllerView"

renderView :: ReactClass props -> props -> ReactElement ViewEventHandler
renderView (TestReactView _ f) props = runReact $ f props
renderView _ _ = error "The ReactClass passed to renderView was not built by mkView"

renderStatefulView :: (ToJSON state, FromJSON state)
                   => ReactClass props -> state -> props -> ReactElement (StatefulViewEventHandler state)
renderStatefulView (TestReactStatefulView _ f) state props = fmap transHandler $ runReact $ f (transState state) props
    where
        transHandler :: (ToJSON s1, FromJSON s1, FromJSON s2, ToJSON s2) => StatefulViewEventHandler s1 -> StatefulViewEventHandler s2
        transHandler h = \handlerState -> let (actions, mstate) = h $ transState handlerState
                                           in (actions, fmap transState mstate)
renderStatefulView _ _ _ = error "The ReactClass passed to renderStatefulView was not built by mkStatefulView"

renderClass :: (ToJSON state, FromJSON state) => ReactClass props -> state -> props -> IO (ReactElement (ClassEventHandler state))
renderClass (TestReactClass _ f) state props = fmap transHandler . runReact <$> f (transState state) props
    where
        transHandler :: (ToJSON s1, FromJSON s1, ToJSON s2, FromJSON s2) => ClassEventHandler s1 -> ClassEventHandler s2
        transHandler h = fmap (fmap transState) . h . transState

renderClass _ _ _ = error "The ReactClass passed to renderClass was not built by mkClass"

transState :: (ToJSON s1, FromJSON s2) => s1 -> s2
transState s1 = case fromJSON $ toJSON s1 of
    Error err -> error $ "Unable to encode and decode state: " ++ err
    Success s2 -> s2

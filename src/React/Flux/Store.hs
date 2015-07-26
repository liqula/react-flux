-- | Internal module containing the store definitions.
module React.Flux.Store (
    ReactStore_
  , ReactStoreRef
  , ReactStore(..)
  , StoreData(..)
  , SomeStoreAction(..)
  , mkStore
  , dispatch
  , dispatchSomeAction
) where

import Control.Concurrent.MVar (MVar, newMVar, modifyMVar_)
import Data.Typeable (Typeable)
import System.IO.Unsafe (unsafePerformIO)

import React.Flux.JsTypes

-- | As part of each store, we have a javascript object with two properties:
--
-- * sdata: holds a value of type @Export storeData@ which is the current data for the store
--
-- * views: an array of @setState@ functions for component views.  The component views
--     add and remove from this property directly inside their lifecycle methods.
--
-- This type is used to represent this foreign javascript object
data ReactStore_
type ReactStoreRef storeData = JSRef ReactStore_

-- | A store contains application state, receives actions from the dispatcher, and notifies
-- component views to re-render themselves.
--
-- A store keeps a global reference to a value of type @storeData@, which must be an instance of
-- 'StoreData'.  When the store receives an action from 'dispatch', it first transforms the data and
-- then notifies all component views to re-render themselves.
-- 
-- You can have multiple stores; it should be the case that all of the state required to render the
-- page is contained in the stores.
--
-- TODO: copy from example
data ReactStore storeData = ReactStore {
    -- | A reference to the foreign javascript part of the store.
    storeRef :: ReactStoreRef storeData

    -- | An MVar containing the current store data.  Normally, the MVar is full and contains the
    -- current store data.  When applying an action, the MVar is kept empty for the entire operation
    -- of transforming to the new data and sending the new data to all component views.  This
    -- effectively operates as a lock allowing only one thread to modify the store at any one time.
    -- This lock is safe because only the 'dispatch' function ever accesses this MVar.
  , storeData :: MVar storeData
}

-- | The data in a store must be an instance of this typeclass.
class Typeable storeData => StoreData storeData where
    -- | The actions that this store accepts
    type StoreAction storeData

    -- | The action that the store uses to transform the data.  Note that if this action
    -- throws an exception, the transform will be aborted and the old store data will be kept
    -- unchanged.
    transform :: StoreAction storeData -> storeData -> IO storeData

-- | An existential type for some store action.  It is used for event handlers in controller-views
-- and classes, so it is helpful to create utility functions creating 'SomeStoreAction'.
data SomeStoreAction = forall storeData. StoreData storeData
    => SomeStoreAction (ReactStore storeData) (StoreAction storeData)

----------------------------------------------------------------------------------------------------
-- mkStore has two versions
----------------------------------------------------------------------------------------------------

-- | Create a new store from the initial data.

#ifdef __GHCJS__

mkStore :: StoreData storeData => storeData -> ReactStore storeData
mkStore initial = unsafePerformIO $ do
    i <- export initial
    storeRef <- jsCreateStore i
    storeMVar <- newMVar initial
    return $ ReactStore storeRef storeMVar

-- | Create the javascript half of the store.
foreign import javascript unsafe
    "{sdata:$1, views: []}"
    js_CreateStore :: Export storeData -> IO (ReactStoreRef storeData)

-- | When updating the store data we need call 'releaseExport' on the old store data, and we
-- want this to be safe even if an exception occurs.  To do so, the update function receives a
-- javascript object with a single property @export@.  Initially, this contains the Export for the
-- new data.  The update first swaps this property with the current data, and then calls out to the
-- component views to notify them of the change.  This type is for this argument object.
data UpdateStoreArgument_
type UpdateStoreArgument = JSRef UpdateStoreArgument_

-- | Create a new argument object to pass to UpdateStore.
foreign import javascript unsafe
    "{export:$1}"
    js_UpdateStore_CreateArgument :: Export storeData -> IO UpdateStoreArgument

-- | Perform the update, swapping the old export and the new export and then notifying the component
-- views.
foreign import javascript unsafe
    "(function(store, arg) { \
        var oldD = store.sdata; \
        store.sdata = arg.export; \
        arg.export = oldD; \
        store.views.map(function(f) { f(store.sdata); }; \
    })($1, $2)"
    js_UpdateStore :: ReactStoreRef storeData -> UpdateStoreArgument -> IO ()

-- | Retrieve the old export so that it can be released.
foreign import javascript unsafe
    "$1.export"
    js_UpdateStore_RetrieveOldExport :: UpdateStoreArgument -> IO (Export storeData)

#else

mkStore :: StoreData storeData => storeData -> ReactStore storeData
mkStore initial = unsafePerformIO $ do
    storeMVar <- newMVar initial
    return $ ReactStore () storeMVar

#endif

----------------------------------------------------------------------------------------------------
-- dispatch has two versions
----------------------------------------------------------------------------------------------------

-- | Dispatch an action to a store.  This first causes the store data to transform according to the
-- action and then notifies all component views that the store data has changed.
--
-- This function uses an MVar to make sure only a single thread is updating the store and
-- re-rendering the view at a single time.  Thus this function can block.

#ifdef __GHCJS__

dispatch :: StoreData storeData => ReactStore storeData -> StoreAction storeData -> IO ()
dispatch store action = modifyMVar_ (storeData store) $ \oldData -> do

    newData <- transform action oldData
    newDataE <- export newData
    
    arg <- js_UpdateStore_CreateArgument newDataE

    js_UpdateStore (storeRef store) arg
        `finally` js_UpdateStore_RetrieveOldState arg >>= releaseExport

    return newData

#else

dispatch :: StoreData storeData => ReactStore storeData -> StoreAction storeData -> IO ()
dispatch store action = modifyMVar_ (storeData store) (transform action)

#endif

-- | Dispatch some store action.
dispatchSomeAction :: SomeStoreAction -> IO ()
dispatchSomeAction (SomeStoreAction store action) = dispatch store action

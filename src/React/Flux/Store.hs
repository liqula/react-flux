-- | Internal module containing the store definitions.
module React.Flux.Store (
    ReactStoreRef(..)
  , ReactStore(..)
  , StoreData(..)
  , SomeStoreAction(..)
  , mkStore
  , dispatch
  , dispatchSomeAction
) where

import Control.Concurrent.MVar (MVar, newMVar, modifyMVar_)
import Control.DeepSeq
import Data.Typeable (Typeable)
import System.IO.Unsafe (unsafePerformIO)

#ifdef __GHCJS__
import GHCJS.Types (JSRef, isNull)
import React.Flux.Export (Export, export)
--import GHCJS.Foreign.Export (Export, export)
#else
type JSRef a = ()
type Export a = ()
#endif

-- | This type is used to represent the foreign javascript object part of the store.
newtype ReactStoreRef storeData = ReactStoreRef (JSRef ())

-- | A store contains application state, receives actions from the dispatcher, and notifies
-- component views to re-render themselves.  You can have multiple stores; it should be the case
-- that all of the state required to render the page is contained in the stores.
--
-- A store keeps a global reference to a value of type @storeData@, which must be an instance of
-- 'StoreData'.  When the store receives an action from 'dispatch', it first transforms the data and
-- then notifies all component views to re-render themselves.
--
-- >data Todo = Todo {
-- >    todoText :: String
-- >  , todoComplete :: Bool
-- >  , todoIsEditing :: Bool
-- >} deriving (Show, Typeable)
-- >
-- >newtype TodoState = TodoState {
-- >    todoList :: [(Int, Todo)]
-- >} deriving (Show, Typeable)
-- >
-- >data TodoAction = TodoCreate String
-- >                | TodoDelete Int
-- >                | TodoEdit Int
-- >                | UpdateText Int String
-- >                | ToggleAllComplete
-- >                | TodoSetComplete Int Bool
-- >                | ClearCompletedTodos
-- >  deriving (Show, Typeable, Generic, NFData)
-- >
-- >instance StoreData TodoState where
-- >    type StoreAction TodoState = TodoAction
-- >    transform action (TodoState todos) = ...
-- >
-- >todoStore :: ReactStore TodoState
-- >todoStore = mkStore $ TodoState
-- >    [ (0, Todo "Learn react" True False)
-- >    , (1, Todo "Learn react-flux" False False)
-- >    ]
-- >
-- >todoA :: TodoAction -> SomeStoreAction
-- >todoA = SomeStoreAction todoStore
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

    -- | Transform the store data according to the action.  This is the only place in your app where
    -- @IO@ should occur.  The transform function should complete quickly, since the UI will not be
    -- re-rendered until the transform is complete.  Therefore, if you need to perform some longer
    -- action, you should fork a thread from inside 'transform'.  The thread can then 'dispatch'
    -- another action with the result of its computation.  This is very common to communicate with
    -- the backend using AJAX.
    --
    -- Note that if the transform throws an exception, the transform will be aborted and the old
    -- store data will be kept unchanged.  The exception will then be thrown from 'dispatch'.
    --
    -- For the best performance, care should be taken in only modifying the part of the store data
    -- that changed (see below for more information on performance).
    transform :: StoreAction storeData -> storeData -> IO storeData

-- | An existential type for some store action.  It is used for event handlers in views, so it is
-- helpful to create utility functions creating 'SomeStoreAction' for your stores, similar to
-- @todoA@ above.  The 'NFData' instance is important for performance, for details see below.
data SomeStoreAction = forall storeData. (StoreData storeData, NFData (StoreAction storeData))
    => SomeStoreAction (ReactStore storeData) (StoreAction storeData)

instance NFData SomeStoreAction where
    rnf (SomeStoreAction _ action) = action `deepseq` ()

----------------------------------------------------------------------------------------------------
-- mkStore has two versions
----------------------------------------------------------------------------------------------------

-- | Create a new store from the initial data.
mkStore :: StoreData storeData => storeData -> ReactStore storeData

#ifdef __GHCJS__

mkStore initial = unsafePerformIO $ do
    i <- export initial
    ref <- js_CreateStore i
    storeMVar <- newMVar initial
    return $ ReactStore ref storeMVar

-- | Create the javascript half of the store.
foreign import javascript unsafe
    "{sdata:$1, views: []}"
    js_CreateStore :: Export storeData -> IO (ReactStoreRef storeData)

-- | Perform the update, swapping the old export and the new export and then notifying the component views.
foreign import javascript unsafe
    "hsreact$transform_store($1, $2)"
    js_UpdateStore :: ReactStoreRef storeData -> Export storeData -> IO ()

#else

mkStore initial = unsafePerformIO $ do
    storeMVar <- newMVar initial
    return $ ReactStore () storeMVar

#endif

{-# NOINLINE mkStore #-}

----------------------------------------------------------------------------------------------------
-- dispatch has two versions
----------------------------------------------------------------------------------------------------

-- | Dispatch an action to a store.  This first causes the store data to transform according to the
-- action and then notifies all component views that the store data has changed.
--
-- This function uses an MVar to make sure only a single thread is updating the store and
-- re-rendering the view at a single time.  Thus this function can block.
dispatch :: StoreData storeData => ReactStore storeData -> StoreAction storeData -> IO ()

#ifdef __GHCJS__

dispatch store action = modifyMVar_ (storeData store) $ \oldData -> do
    newData <- transform action oldData

    -- There is a hack in PropertiesAndEvents that the fake event store for propagation and prevent
    -- default does not have a javascript store, so the store is nullRef.
    case storeRef store of
        ReactStoreRef ref | not $ isNull ref -> do
            newDataE <- export newData
            js_UpdateStore (storeRef store) newDataE
        _ -> return ()

    return newData

#else

dispatch store action = modifyMVar_ (storeData store) (transform action)

#endif

-- | Dispatch some store action.
dispatchSomeAction :: SomeStoreAction -> IO ()
dispatchSomeAction (SomeStoreAction store action) = dispatch store action

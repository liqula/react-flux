{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}

-- |

module RouteExample.Types where

import           React.Flux


import qualified Data.Text     as T
import           Data.Typeable (Typeable)

type AppName = String
type AppView = ReactElementM ViewEventHandler
type AppRouter = [T.Text] -> IO ()

data App props = forall state. StoreData state =>
           App {appName        :: AppName
               , appState      :: ReactStore state
               , appView       :: Typeable props => state -> props -> AppView ()
               , appInitAction :: StoreAction state
               , appRouter     :: Maybe AppRouter
               }
               deriving Typeable

initApp :: Typeable props => App props -> IO (ReactView props)
initApp App{..} = do
  let view' = defineControllerView appName appState (\st props -> appView st props)
  alterStore appState appInitAction
  return view'

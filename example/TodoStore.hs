{-# LANGUAGE TypeFamilies #-}
module TodoStore where

import React.Flux
import Data.Typeable (Typeable)

data Todo = Todo {
    todoText :: String
  , todoComplete :: Bool
} deriving (Show, Typeable)

newtype TodoState = TodoState {
    todoList :: [(Int, Todo)]
} deriving (Show, Typeable)

data TodoAction = TodoCreate String
                 | TodoDelete Int
                 | UpdateText Int String
                 | ToggleAllComplete
                 | TodoSetComplete Int Bool
  deriving (Show, Typeable)

instance StoreData TodoState where
    type StoreAction TodoState = TodoAction
    transform action (TodoState todos) = undefined

todoStore :: ReactStore TodoState
todoStore = mkStore $ TodoState []

todoA :: TodoAction -> SomeStoreAction
todoA = SomeStoreAction todoStore

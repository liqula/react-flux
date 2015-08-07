{-# LANGUAGE TypeFamilies, DeriveGeneric, DeriveAnyClass #-}
module TodoStore where

import React.Flux
import Control.DeepSeq
import GHC.Generics (Generic)
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
  deriving (Show, Typeable, Generic, NFData)

instance StoreData TodoState where
    type StoreAction TodoState = TodoAction
    transform action (TodoState todos) = do
        putStrLn $ "Action: " ++ show action
        putStrLn $ "Initial todos: " ++ show todos
        newTodos <- return $  case action of
            (TodoCreate txt) -> (maximum (map fst todos) + 1, Todo txt False) : todos
            (TodoDelete i) -> filter ((/=i) . fst) todos
            (UpdateText newIdx newTxt) -> [ (idx, Todo (if idx == newIdx then newTxt else txt) complete)
                                          | (idx, Todo txt complete) <- todos
                                          ]
            ToggleAllComplete -> [ (idx, Todo txt True) | (idx, Todo txt _) <- todos ]
            TodoSetComplete newIdx newComplete -> [ (idx, Todo txt (if idx == newIdx then newComplete else complete))
                                                  | (idx, Todo txt complete) <- todos
                                                  ]
        putStrLn $ "New todos: " ++ show newTodos
        return $ TodoState newTodos

todoStore :: ReactStore TodoState
todoStore = mkStore $ TodoState
    [ (0, Todo "Learn react" True)
    , (1, Todo "Learn react-flux" False)
    ]

todoA :: TodoAction -> SomeStoreAction
todoA = SomeStoreAction todoStore

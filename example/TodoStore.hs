{-# LANGUAGE TypeFamilies, DeriveGeneric, DeriveAnyClass #-}
module TodoStore where

import React.Flux
import Control.DeepSeq
import GHC.Generics (Generic)
import Data.Typeable (Typeable)

data Todo = Todo {
    todoText :: String
  , todoComplete :: Bool
  , todoIsEditing :: Bool
} deriving (Show, Typeable)

newtype TodoState = TodoState {
    todoList :: [(Int, Todo)]
} deriving (Show, Typeable)

data TodoAction = TodoCreate String
                | TodoDelete Int
                | TodoEdit Int
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
            (TodoCreate txt) -> (maximum (map fst todos) + 1, Todo txt False False) : todos
            (TodoDelete i) -> filter ((/=i) . fst) todos
            (TodoEdit i) -> [ (idx, Todo txt complete (idx == i)) | (idx, Todo txt complete _) <- todos ]
            (UpdateText newIdx newTxt) -> [ (idx, Todo (if idx == newIdx then newTxt else txt) complete False)
                                          | (idx, Todo txt complete _) <- todos
                                          ]
            ToggleAllComplete -> [ (idx, Todo txt True False) | (idx, Todo txt _ _) <- todos ]
            TodoSetComplete newIdx newComplete -> [ (idx, Todo txt (if idx == newIdx then newComplete else complete) False)
                                                  | (idx, Todo txt complete _) <- todos
                                                  ]
        putStrLn $ "New todos: " ++ show newTodos
        return $ TodoState newTodos

todoStore :: ReactStore TodoState
todoStore = mkStore $ TodoState
    [ (0, Todo "Learn react" True False)
    , (1, Todo "Learn react-flux" False False)
    ]

todoA :: TodoAction -> SomeStoreAction
todoA = SomeStoreAction todoStore

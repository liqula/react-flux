module TodoStore where

data Todo = Todo {
    todoText :: String
  , todoComplete :: Bool
} deriving (Show, Typeable)

newtype TodoState = TodoState {
    todoList :: [(Int, Todo)]
  , todoPartialNew
} deriving (Show, Typeable)

data TodoActions = TodoCreate String
                 | TodoToggleAllComplete
                 | TodoComplete Int
                 | TodoUndoComplete Int
                 | TodoUpdateText Int String
                 | TodoDelete Int
  deriving (Show, Typeable)

instance StoreData TodoState where
    type StoreAction TodoState = TodoActions
    transform (TodoState todos) action = ...

todoStore :: ReactStore TodoState
todoStore = mkStore $ TodoState []

todoCreateAction :: String -> SomeStoreAction
todoCreateAction txt = SomeStoreAction todoStore (TodoCreate txt)


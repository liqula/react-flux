module TodoViews where

import React.Flux

import TodoStore
import TodoComponents

todoApp :: ReactClass ()
todoApp = mkControllerView "todo app" todoStore $ \todoState () ->
    div_ $ do
        header_
        mainSection_ todoState
        footer_ todoState

header :: ReactClass ()
header = mkView "header" $ \() ->
    header_ ["id" @= "header"] $ do
        h1_ "todos" mempty
        todoTextInput_  TextInputArgs
          { tiaId = Just "new-todo"
          , tiaClass = "new-todo"
          , tiaPlaceholder = "What needs to be done?"
          , tiaOnSave = todoA . TodoCreate
          , tiaValue = Nothing
          }

header_ :: ReactElementM eventHandler a
header_ = rclass header ()

mainSection_ :: TodoState -> ReactElementM ViewEventHandler a
mainSection_ st = section_ ["id" @= "main"] $ do
    input_ [ "id" @= "toggle-all"
           , "type" @= "checkbox"
           , "checked" @= if all (map todoComplete $ todoList st) then "checked" else ""
           , onChange $ \_ _ -> todoA ToggleAllComplete
           ] mempty

    label_ [ "htmlFor" @= "toggle-all"] "Mark all as complete"
    ul_ [ "id" @= "todo-list" ] $ mapM_ todoItem_ $ todoList st

todoItem :: ReactClass (Int, Todo)
todoItem = mkStatefulView False $ \isEditing (todoIdx, todo) ->
    li_ [ "className" @= intercalate "," ([ "completed" | todoComplete todo] ++ [ "editing" | isEditing ])
        , "key" @= todoIdx
        ] $ do
        
        div_ [ "className" @= "view"]
            input_ [ "className" @= "toggle"
                   , "type" @= "checkbox"
                   , "checked" @= todoComplete todo
                   , onChange $ \_ _ -> todoA $ TodoSetComplete todoIdx $ not $ todoComplete todo
                   ]

            label_ [ onDoubleClick $ \_ -> ([], Just True) ] (fromString $ todoText todo)

            button_ [ "className" @= "destroy"
                    , onClick $ \_ _ -> todoA $ TodoDelete todoIdx
                    ]

        when (isEditing todo) $
            todoTextInput_ TextInputArgs
                { tiaId = Nothing
                , tiaClass = "edit"
                , tiaPlaceholder = ""
                , tiaOnSave = todoA . UpdateText todoIdx
                , tiaValue = todoText todo
                }

todoItem_ :: (Int, Todo) -> ReactElementM eventHandler a
todoItem_ (idx, todo) = rclassWithKey todoItem idx (idx, todo) mempty

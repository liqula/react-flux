module TodoView where

-- >todoApp :: ReactClass ()
-- >todoApp = mkControllerView "todoApp" todoStore $ \todos [] ->
-- >    el "div"
-- >        [ header
-- >        , mainSection todos
-- >        , footer todos
-- >        ]
-- >
-- >header :: ReactElement eventHandler
-- >header =
-- >    el "header"
-- >        [ el "h1" []
-- >            [ text "Todos"]
-- >        , class todoTextInput TextInputProps
-- >            { placeholder = "What needs to be done?"
-- >            , onSave = todoCreateAction
-- >            }
-- >        ]
-- >
-- >mainSection :: TodoState -> ReactElement ControllerViewEventHandler
-- >mainSection (TodoState todos) =
-- >    el "section" []
-- >        [ input "checkbox"
-- >            [ id .= "toggle-all"
-- >            , checked .= if all (map todoComplete todos) then "checked" else ""
-- >            ]
-- >            [ evt "onChange" $ const todoToggleAllCompleted ]
-- >        , el "label" ["htmlFor" .= "toggle-all"]
-- >            [ text "Mark all as complete" ]
-- >        , el "ul" [] $ map todoItem 
-- >     TODOTODO
{-# LANGUAGE OverloadedStrings #-}
module TodoSpec (spec) where

import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text              as T
import           System.Directory       (getCurrentDirectory)
import           Test.Hspec.WebDriver

expectTodos :: [(T.Text, Bool)] -> WD ()
expectTodos todos = do
    entries <- findElems $ ByCSS "ul#todo-list > li"
    length entries `shouldBe` length todos

    -- check todos
    forM_ (zip entries todos) $ \(li, (todo, complete)) -> do
        chk <- findElemFrom li $ ByCSS "input[type=checkbox]"
        attr chk "checked" `shouldReturn` if complete then Just "true" else Nothing
        spn <- findElemFrom li $ ByTag "label"
        getText spn `shouldReturn` todo

    -- check items left
    let cnt = length $ filter (not . snd) todos
    cntSpan <- findElem $ ByCSS "span#todo-count"

    if cnt == 1
        then getText cntSpan `shouldReturn` "1 item left"
        else getText cntSpan `shouldReturn` (T.pack $ show cnt ++ " items left")

    -- clear completed
    let completedCnt = length $ filter snd todos
    when (completedCnt > 0) $ do
        completeBtn <- findElem $ ByCSS "button#clear-completed"
        getText completeBtn `shouldReturn` (T.pack $ "Clear completed (" ++ show completedCnt ++ ")")

getRow :: Int -> WD Element
getRow i = do
    entries <- findElems $ ByCSS "ul#todo-list > li"
    return $ entries !! i

allBrowsers :: [Capabilities]
allBrowsers = [chromeCaps]

spec :: Spec
spec = session " for todo example application" $ using allBrowsers $ do
    it "opens the page" $ runWD $ do
        dir <- liftIO $ getCurrentDirectory
        openPage $ "file://" ++ dir ++ "/../../example/todo/todo.html"
        expectTodos [("Learn react", True), ("Learn react-flux", False)]

    it "adds a new todo via blur" $ runWD $ do
        txt <- findElem $ ByCSS "input#new-todo"
        sendKeys "Test react-flux" txt
        findElem (ByCSS "header#header h1") >>= click
        expectTodos [("Test react-flux", False), ("Learn react", True), ("Learn react-flux", False)]

    it "marks a todo as completed" $ runWD $ do
        lastRow <- getRow 2
        findElemFrom lastRow (ByCSS "input[type=checkbox]") >>= click
        expectTodos [("Test react-flux", False), ("Learn react", True), ("Learn react-flux", True)]

    it "edits a todo" $ runWD $ do
        midRow <- getRow 1
        findElemFrom midRow (ByTag "label") >>= moveToCenter
        doubleClick
        editBox <- findElemFrom midRow (ByCSS "input.edit")
        sendKeys "Learn react.js" editBox
        findElem (ByCSS "header#header h1") >>= click
        expectTodos [("Test react-flux", False), ("Learn react.js", True), ("Learn react-flux", True)]

    it "clears all completed todos" $ runWD $ do
        findElem (ByCSS "button#clear-completed") >>= click
        expectTodos [("Test react-flux", False)]

    it "adds a todo via enter key" $ runWD $ do
        txt <- findElem $ ByCSS "input#new-todo"
        sendKeys "Party\xE007" txt
        expectTodos [("Party", False), ("Test react-flux", False)]

    it "marks all todos as complete" $ runWD $ do
        findElem (ByCSS "input#toggle-all") >>= click
        expectTodos [("Party", True), ("Test react-flux", True)]

    it "deletes a todo" $ runWD $ do
        lastRow <- getRow 1
        moveToCenter lastRow
        btn <- findElemFrom lastRow (ByCSS "button.destroy")
        click btn
        expectTodos [("Party", True)]

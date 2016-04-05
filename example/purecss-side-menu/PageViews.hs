{-# LANGUAGE OverloadedStrings #-}

-- | The pages.  Normally each page should go into its own module, but for simplicity in this
-- example they are all defined here.
module PageViews where

import React.Flux
import Dispatcher

page1 :: ReactView ()
page1 = defineView "page 1" $ \() -> div_ $ do
    cldiv_ "header" $
        h1_ "Page 1!!"
    cldiv_ "content" $ do
        p_ $ do
            "Page 1 content. "
            clbutton_ "pure-button" (changePageTo Page2) "Change to page 2"
            "Also, try reducing the browser width to see the responsive menu."
        p_ $ do
            "You must load this file from a server.  If you did not, page changes might not work depending on your browser security settings (some features don't always work for file URLs).  Use for example "
            code_ "python3 -m http.server 8000"
            " or "
            code_ "python2 -m SimpleHTTPServer 8000"
            " from the example/purecss-side-menu directory."

page2 :: ReactView ()
page2 = defineView "page 2" $ \() -> div_ $ do
    cldiv_ "header" $
        h1_ "Just page 2"
    cldiv_ "content" $
        p_ "Page 2 content. Try the back button."

page3 :: ReactView ()
page3 = defineView "page 3" $ \() -> div_ $ do
    cldiv_ "header" $
        h1_ "Page Three"
    cldiv_ "content" $
        p_ "Page 3 content. Try the back button."

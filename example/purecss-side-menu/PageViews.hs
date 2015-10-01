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
            a_ ["href" $= "#", onClick $ \_ _ -> changePageTo Page2] "Change to page 2. "
            "Also, try reducing the browser width to see the responsive menu."

page2 :: ReactView ()
page2 = defineView "page 2" $ \() -> div_ $ do
    cldiv_ "header" $
        h1_ "Just page 2"
    cldiv_ "content" $
        p_ "Page 2 content."

page3 :: ReactView ()
page3 = defineView "page 3" $ \() -> div_ $ do
    cldiv_ "header" $
        h1_ "Page Three"
    cldiv_ "content" $
        p_ "Page 3 content."
